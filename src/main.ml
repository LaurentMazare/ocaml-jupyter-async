open Core
open Async

let buf_len = 4096

module Config = struct
  type t =
    { control_port : int
    ; shell_port : int
    ; transport : string
    ; signature_scheme : string
    ; stdin_port : int
    ; hb_port : int
    ; ip : string
    ; iopub_port : int
    ; key : string
    }
  [@@deriving yojson, sexp] [@@yojson.allow_extra_fields]

  let socket t which ~context ~kind =
    let socket = Zmq.Socket.create context kind in
    let port =
      match which with
      | `hb -> t.hb_port
      | `control -> t.control_port
      | `shell -> t.shell_port
      | `stdin -> t.stdin_port
      | `iopub -> t.iopub_port
    in
    let url = sprintf "%s://%s:%d" t.transport t.ip port in
    Zmq.Socket.bind socket url;
    Zmq_async.Socket.of_socket socket
end

type t =
  { config : Config.t
  ; hb_socket : [ `Rep ] Zmq_async.Socket.t
  ; control_socket : [ `Router ] Zmq_async.Socket.t
  ; shell_socket : [ `Router ] Zmq_async.Socket.t
  ; stdin_socket : [ `Router ] Zmq_async.Socket.t
  ; iopub_socket : [ `Pub ] Zmq_async.Socket.t
  ; mutable execution_count : int
  ; mutable last_parent_header : Message.Header.t option
  ; worker : Worker.t
  }

let close t =
  let%bind () = Zmq_async.Socket.close t.hb_socket in
  let%bind () = Zmq_async.Socket.close t.iopub_socket in
  Deferred.List.iter
    ~f:Zmq_async.Socket.close
    [ t.control_socket; t.shell_socket; t.stdin_socket ]

let hb_loop t =
  let rec loop () =
    let%bind msg = Zmq_async.Socket.recv t.hb_socket in
    Log.Global.debug "Received heartbeat message: %s\n%!" msg;
    loop ()
  in
  loop ()

let control_loop t =
  let rec loop () =
    let%bind msg = Message.read t.control_socket ~key:t.config.key in
    Log.Global.debug_s ~tags:[ "kind", "shell" ] (Message.sexp_of_t msg);
    loop ()
  in
  loop ()

let handle_shell t (msg : Message.t) =
  match msg.header.msg_type with
  | "kernel_info_request" ->
    let msg = Message.kernel_info_reply msg in
    Message.send msg t.shell_socket ~key:t.config.key
  | "comm_info_request" ->
    let msg = Message.comm_info_reply msg in
    Message.send msg t.shell_socket ~key:t.config.key
  | "shutdown_request" ->
    let msg = Message.shutdown_reply msg in
    let%bind () = Message.send msg t.shell_socket ~key:t.config.key in
    let%map () = close t in
    Async.shutdown 0
  | "execute_request" ->
    t.last_parent_header <- Some msg.header;
    let execute_request = Message.Execute_request_content.t_of_yojson msg.content in
    Log.Global.debug_s (Message.Execute_request_content.sexp_of_t execute_request);
    let%bind () =
      Message.send
        (Message.status Busy ~parent_header:msg.header)
        t.iopub_socket
        ~key:t.config.key
    in
    let%bind result = Worker.execute t.worker ~code:execute_request.code in
    (* For now use the toploop in the same process. This may not work well as
       calls are likely to be blocking if we want to allow Async computations in
       the executed code.  *)
    let%bind status, user_expressions =
      match result with
      | Ok () ->
        t.execution_count <- t.execution_count + 1;
        return (Message.Execute_reply_content.Ok, Some [])
      | Error err ->
        Log.Global.debug "error in toploop: %s" (Error.to_string_hum err);
        let%bind () =
          Message.send
            (Message.stream Stderr (Error.to_string_hum err) ~parent_header:msg.header)
            t.iopub_socket
            ~key:t.config.key
        in
        return (Message.Execute_reply_content.Error, None)
    in
    let msg =
      Message.execute_reply
        msg
        ~status
        ~execution_count:t.execution_count
        ~user_expressions
    in
    let%bind () = Message.send msg t.shell_socket ~key:t.config.key in
    Message.send
      (Message.status Idle ~parent_header:msg.header)
      t.iopub_socket
      ~key:t.config.key
  | _ -> Deferred.unit

let shell_loop t =
  let rec loop () =
    let%bind msg = Message.read t.shell_socket ~key:t.config.key in
    Log.Global.debug_s ~tags:[ "kind", "shell" ] (Message.sexp_of_t msg);
    let%bind () = handle_shell t msg in
    loop ()
  in
  loop ()

let iopub_loop t =
  let rec loop () =
    let%bind msg = Zmq_async.Socket.recv t.iopub_socket in
    Log.Global.debug "Received iopub message: %s\n%!" msg;
    loop ()
  in
  loop ()

let stdin_loop t =
  let rec loop () =
    let%bind msg = Zmq_async.Socket.recv t.stdin_socket in
    Log.Global.debug "Received stdin message: %s\n%!" msg;
    loop ()
  in
  loop ()

let redirect_loop t which ~reader =
  let buf = Bytes.create buf_len in
  let rec loop () =
    let%bind bytes_read = Reader.read reader buf in
    match bytes_read with
    | `Eof -> Deferred.unit
    | `Ok bytes_read ->
      let%bind () =
        match t.last_parent_header with
        | None -> Deferred.unit
        | Some parent_header ->
          let text = Bytes.sub buf ~pos:0 ~len:bytes_read |> Bytes.to_string in
          Message.send
            (Message.stream which text ~parent_header)
            t.iopub_socket
            ~key:t.config.key
      in
      loop ()
  in
  loop ()

let stdout_loop t =
  let reader = Worker.stdout_reader t.worker in
  redirect_loop t Stdout ~reader

let stderr_loop t =
  let reader = Worker.stderr_reader t.worker in
  redirect_loop t Stderr ~reader

let register_printer () =
  Caml.Printexc.register_printer (function
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Exn.to_string exn |> Option.some
      | _ -> None)

let run config =
  let context = Zmq.Context.create () in
  (* It is import to call this before entering async. *)
  let worker = Worker.spawn () in
  let t =
    { config
    ; hb_socket = Config.socket config `hb ~context ~kind:Zmq.Socket.rep
    ; control_socket = Config.socket config `control ~context ~kind:Zmq.Socket.router
    ; shell_socket = Config.socket config `shell ~context ~kind:Zmq.Socket.router
    ; stdin_socket = Config.socket config `stdin ~context ~kind:Zmq.Socket.router
    ; iopub_socket = Config.socket config `iopub ~context ~kind:Zmq.Socket.pub
    ; execution_count = 0
    ; worker
    ; last_parent_header = None
    }
  in
  Deferred.all_unit
    [ hb_loop t
    ; control_loop t
    ; shell_loop t
    ; iopub_loop t
    ; stdin_loop t
    ; stdout_loop t
    ; stderr_loop t
    ]

let command =
  Core.Command.basic
    ~summary:"an ocaml kernel for jupyter"
    (let%map_open.Command connection_file =
       flag "-connection-file" (required string) ~doc:"a connection file in json format"
     in
     fun () ->
       register_printer ();
       Log.Global.set_level `Debug;
       let run = Yojson.Safe.from_file connection_file |> Config.t_of_yojson |> run in
       don't_wait_for run;
       Scheduler.go () |> never_returns)
