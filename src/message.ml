open Core
open Async

let protocol_version = "5.3"
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
    ; protocol_version
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
    | Ok
    | Error
    | Aborted
  [@@deriving sexp]

  let status_of_yojson = function
    | `String "ok" -> Ok
    | `String "error" -> Error
    | `String "aborted" -> Aborted
    | _ -> failwith "unexpected status"

  let yojson_of_status = function
    | Ok -> `String "ok"
    | Error -> `String "error"
    | Aborted -> `String "aborted"

  type t =
    { status : status
    ; execution_count : int
    ; user_expressions : string json_assoc option [@yojson.option]
    }
  [@@deriving sexp, yojson] [@@yojson.allow_extra_fields]
end

module Stream_content = struct
  type name =
    | Stdout
    | Stderr
  [@@deriving sexp]

  let name_of_yojson = function
    | `String "stdout" -> Stdout
    | `String "stderr" -> Stderr
    | _ -> failwith "unexpected name"

  let yojson_of_name = function
    | Stdout -> `String "stdout"
    | Stderr -> `String "stderr"

  type t =
    { name : name
    ; text : string
    }
  [@@deriving sexp, yojson]
end

module Status_content = struct
  type execution_state =
    | Busy
    | Idle
    | Starting
  [@@deriving sexp]

  let execution_state_of_yojson = function
    | `String "busy" -> Busy
    | `String "idle" -> Idle
    | `String "starting" -> Starting
    | _ -> failwith "unexpected execution state"

  let yojson_of_execution_state = function
    | Busy -> `String "busy"
    | Idle -> `String "idle"
    | Starting -> `String "starting"

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

let reply ~ids ~msg_type ~parent_header ~content =
  { ids
  ; header =
      { parent_header with msg_id = Uuid_unix.create () |> Uuid.to_string; msg_type }
  ; parent_header = Header.yojson_of_t parent_header
  ; metadata = `Assoc []
  ; content
  ; buffers = []
  }

let status execution_state ~parent_header =
  reply
    ~ids:[ "kernel-status" ]
    ~msg_type:"status"
    ~parent_header
    ~content:(Status_content.yojson_of_t { execution_state })

let stream name text ~parent_header =
  reply
    ~ids:[ "kernel-stream" ]
    ~msg_type:"stream"
    ~parent_header
    ~content:(Stream_content.yojson_of_t { name; text })

let kernel_info_reply t =
  reply
    ~ids:t.ids
    ~msg_type:"kernel_info_reply"
    ~parent_header:t.header
    ~content:Kernel_info_reply_content.(default () |> yojson_of_t)

let comm_info_reply t =
  reply
    ~ids:t.ids
    ~msg_type:"comm_info_reply"
    ~parent_header:t.header
    ~content:(`Assoc [ "comms", `Assoc [] ])

let shutdown_reply t =
  reply ~ids:t.ids ~msg_type:"shutdown_reply" ~parent_header:t.header ~content:t.content
