open Core
open Async

module Message = struct
  let delimiter = "<IDS|MSG>"

  type 'a json_assoc = (string * 'a) list [@@deriving sexp]

  let json_assoc_of_yojson of_yojson = function
    | `Assoc l -> List.map l ~f:(fun (key, value) -> key, of_yojson value)
    | _other -> failwith "expected an Assoc"

  let yojson_of_json_assoc yojson_of l =
    `Assoc (List.map l ~f:(fun (name, value) -> name, yojson_of value))

  module Hmac = struct
    let hexa_encode = Cryptokit.Hexa.encode ()

    let hmac ~key ~header ~parent_header ~metadata ~content =
      let hmac = Cryptokit.MAC.hmac_sha256 key in
      hmac#add_string header;
      hmac#add_string parent_header;
      hmac#add_string metadata;
      hmac#add_string content;
      Cryptokit.transform_string hexa_encode hmac#result
  end

  module Header = struct
    type t =
      { msg_id : string
      ; session : string
      ; username : string
      ; date : string option [@yojson.option]
      ; msg_type : string
      ; version : string
      }
    [@@deriving sexp_of, yojson] [@@yojson.allow_extra_fields]
  end

  module Kernel_info_reply_content = struct
    type language_info =
      { name : string
      ; version : string
      ; mimetype : string
      ; file_extension : string
      ; pygments_lexer : string option [@yojson.option]
      ; codemirror_mode : string option [@yojson.option]
      ; nbconvert_exporter : string option [@yojson.option]
      }
    [@@deriving yojson]

    type help_link =
      { text : string
      ; url : string
      }
    [@@deriving yojson]

    type t =
      { status : string
      ; protocol_version : string
      ; implementation : string
      ; implementation_version : string
      ; language_info : language_info
      ; banner : string
      ; help_links : help_link list
      }
    [@@deriving yojson]

    let default () =
      { status = "ok"
      ; protocol_version = "5.3"
      ; implementation = "ocaml-jupyter-async"
      ; implementation_version = "1.0"
      ; language_info =
          { name = "ocaml"
          ; version = Sys.ocaml_version
          ; mimetype = "text/x-ocaml"
          ; file_extension = ".ml"
          ; pygments_lexer = None
          ; codemirror_mode = None
          ; nbconvert_exporter = None
          }
      ; banner = "ocaml " ^ Sys.ocaml_version
      ; help_links = []
      }
  end

  module Execute_request_content = struct
    type t =
      { code : string
      ; silent : bool [@default false]
      ; store_history : bool option [@yojson.option]
      ; user_expressions : string json_assoc
      ; allow_stdin : bool
      ; stop_on_error : bool
      }
    [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
  end

  module Execute_reply_content = struct
    type status =
      | Ok [@name "ok"]
      | Error [@name "error"]
      | Aborted [@name "aborted"]
    [@@deriving sexp, yojson]

    type t =
      { status : status
      ; execution_count : int
      ; user_expressions : string json_assoc option [@yojson.option]
      }
    [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
  end

  module Stream_content = struct
    type name =
      | Stdout [@name "stdout"]
      | Stderr [@name "stderr"]
    [@@deriving sexp, yojson]

    type t =
      { name : name
      ; text : string
      }
    [@@deriving sexp, yojson]
  end

  module Status_content = struct
    type execution_state =
      | Busy [@name "busy"]
      | Idle [@name "idle"]
      | Starting [@name "starting"]
    [@@deriving sexp, yojson]

    type t = { execution_state : execution_state } [@@deriving sexp, yojson]
  end

  type t =
    { ids : string list
    ; header : Header.t
    ; parent_header : (Yojson.Safe.t[@sexp.opaque])
    ; metadata : (Yojson.Safe.t[@sexp.opaque])
    ; content : (Yojson.Safe.t[@sexp.opaque])
    ; buffers : string list
    }
  [@@deriving sexp_of]

  let read socket ~key =
    let%map messages = Zmq_async.Socket.recv_all socket in
    let rec loop_until_delimiter ~acc = function
      | [] -> failwith "did not find the delimiter message"
      | delim :: tail when String.( = ) delim delimiter -> List.rev acc, tail
      | id :: tail -> loop_until_delimiter ~acc:(id :: acc) tail
    in
    let ids, tail = loop_until_delimiter ~acc:[] messages in
    match tail with
    | hmac_ :: header :: parent_header :: metadata :: content :: buffers ->
      let hmac = Hmac.hmac ~key ~header ~parent_header ~metadata ~content in
      if String.( <> ) hmac hmac_ then failwithf "signature mismatch %s %s" hmac hmac_ ();
      { ids
      ; header = Yojson.Safe.from_string header |> Header.t_of_yojson
      ; parent_header = Yojson.Safe.from_string parent_header
      ; metadata = Yojson.Safe.from_string metadata
      ; content = Yojson.Safe.from_string content
      ; buffers
      }
    | _ -> failwithf "not enough parts in message (%d)" (List.length tail) ()

  let send t socket ~key =
    let { ids; header; parent_header; metadata; content; buffers } = t in
    let header = Header.yojson_of_t header |> Yojson.Safe.to_string in
    let parent_header = Yojson.Safe.to_string parent_header in
    let metadata = Yojson.Safe.to_string metadata in
    let content = Yojson.Safe.to_string content in
    let hmac = Hmac.hmac ~key ~header ~parent_header ~metadata ~content in
    Zmq_async.Socket.send_all
      socket
      (ids
      @ (delimiter :: hmac :: header :: parent_header :: metadata :: content :: buffers))

  let status execution_state ~parent_header =
    let header =
      { parent_header with
        Header.msg_id = Uuid_unix.create () |> Uuid.to_string
      ; msg_type = "status"
      }
    in
    { ids = [ "kernel-status" ]
    ; header
    ; parent_header = Header.yojson_of_t parent_header
    ; metadata = `Assoc []
    ; content = Status_content.yojson_of_t { execution_state }
    ; buffers = []
    }

  let stream name text ~parent_header =
    let header =
      { parent_header with
        Header.msg_id = Uuid_unix.create () |> Uuid.to_string
      ; msg_type = "stream"
      }
    in
    { ids = [ "kernel-stream" ]
    ; header
    ; parent_header = Header.yojson_of_t parent_header
    ; metadata = `Assoc []
    ; content = Stream_content.yojson_of_t { name; text }
    ; buffers = []
    }
end

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
  let send_reply_msg ~msg_type ~content =
    let msg =
      { Message.ids = msg.ids
      ; header =
          { msg.header with msg_id = Uuid_unix.create () |> Uuid.to_string; msg_type }
      ; parent_header = msg.header |> Message.Header.yojson_of_t
      ; metadata = `Assoc []
      ; content
      ; buffers = []
      }
    in
    Log.Global.debug "sending %s reply" msg_type;
    Message.send msg t.shell_socket ~key:t.config.key
  in
  match msg.header.msg_type with
  | "kernel_info_request" ->
    send_reply_msg
      ~msg_type:"kernel_info_reply"
      ~content:Message.Kernel_info_reply_content.(default () |> yojson_of_t)
  | "comm_info_request" ->
    send_reply_msg ~msg_type:"comm_info_reply" ~content:(`Assoc [ "comms", `Assoc [] ])
  | "shutdown_request" ->
    let%bind () = send_reply_msg ~msg_type:"shutdown_reply" ~content:msg.content in
    let%map () = close t in
    Async.shutdown 0
  | "execute_request" ->
    let execute_request = Message.Execute_request_content.t_of_yojson msg.content in
    Log.Global.debug_s (Message.Execute_request_content.sexp_of_t execute_request);
    let%bind () =
      Message.send
        (Message.status Busy ~parent_header:msg.header)
        t.iopub_socket
        ~key:t.config.key
    in
    let%bind () =
      Message.send
        (Message.stream Stdout "foobar\n" ~parent_header:msg.header)
        t.iopub_socket
        ~key:t.config.key
    in
    let%bind result = Worker.execute t.worker ~code:execute_request.code in
    (* For now use the toploop in the same process. This may not work well as
       calls are likely to be blocking if we want to allow Async computations in
       the executed code.  *)
    let (reply : Message.Execute_reply_content.t) =
      match result with
      | Ok () ->
        t.execution_count <- t.execution_count + 1;
        { status = Ok; execution_count = t.execution_count; user_expressions = Some [] }
      | Error err ->
        Log.Global.debug "error in toploop: %s" (Error.to_string_hum err);
        (* Support aborted ? *)
        { status = Error; execution_count = t.execution_count; user_expressions = None }
    in
    let%bind () =
      send_reply_msg
        ~msg_type:"execute_reply"
        ~content:(Message.Execute_reply_content.yojson_of_t reply)
    in
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
    }
  in
  Deferred.all_unit
    [ hb_loop t; control_loop t; shell_loop t; iopub_loop t; stdin_loop t ]

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
