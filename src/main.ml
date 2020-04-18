open Core
open Async

module Message = struct
  let delimiter = "<IDS|MSG>"

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
      ; date : string
      ; msg_type : string
      ; version : string
      }
    [@@deriving sexp_of, yojson]
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
end

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

let zmq_socket t which ~context ~kind =
  let socket = Zmq.Socket.create context kind in
  let port =
    match which with
    | `heartbeat -> t.hb_port
    | `control -> t.control_port
    | `shell -> t.shell_port
    | `stdin -> t.stdin_port
    | `iopub -> t.iopub_port
  in
  let url = sprintf "%s://%s:%d" t.transport t.ip port in
  Zmq.Socket.bind socket url;
  Zmq_async.Socket.of_socket socket

let hb_loop t ~context =
  let socket = zmq_socket t `heartbeat ~context ~kind:Zmq.Socket.rep in
  let rec loop () =
    let%bind msg = Zmq_async.Socket.recv socket in
    Core.printf "Received heartbeat message: %s\n%!" msg;
    loop ()
  in
  loop ()

let control_loop t ~context =
  let socket = zmq_socket t `control ~context ~kind:Zmq.Socket.router in
  let rec loop () =
    let%bind msg = Message.read socket ~key:t.key in
    Log.Global.debug_s ~tags:[ "kind", "shell" ] (Message.sexp_of_t msg);
    loop ()
  in
  loop ()

let shell_loop t ~context =
  let socket = zmq_socket t `shell ~context ~kind:Zmq.Socket.router in
  let rec loop () =
    let%bind msg = Message.read socket ~key:t.key in
    Log.Global.debug_s ~tags:[ "kind", "shell" ] (Message.sexp_of_t msg);
    loop ()
  in
  loop ()

let iopub_loop t ~context =
  let socket = zmq_socket t `iopub ~context ~kind:Zmq.Socket.pub in
  let rec loop () =
    let%bind msg = Zmq_async.Socket.recv socket in
    Core.printf "Received iopub message: %s\n%!" msg;
    loop ()
  in
  loop ()

let stdin_loop t ~context =
  let socket = zmq_socket t `stdin ~context ~kind:Zmq.Socket.pub in
  let rec loop () =
    let%bind msg = Zmq_async.Socket.recv socket in
    Core.printf "Received stdin message: %s\n%!" msg;
    loop ()
  in
  loop ()

let register_printer () =
  Caml.Printexc.register_printer (function
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Exn.to_string exn |> Option.some
      | _ -> None)

let run (t : t) =
  let context = Zmq.Context.create () in
  Deferred.all_unit
    [ hb_loop t ~context
    ; control_loop t ~context
    ; shell_loop t ~context
    ; iopub_loop t ~context
    ; stdin_loop t ~context
    ]

let command =
  Command.async
    ~summary:"an ocaml kernel for jupyter"
    (let%map_open.Command connection_file =
       flag "-connection-file" (required string) ~doc:"a connection file in json format"
     in
     fun () ->
       register_printer ();
       Log.Global.set_level `Debug;
       Yojson.Safe.from_file connection_file |> t_of_yojson |> run)
