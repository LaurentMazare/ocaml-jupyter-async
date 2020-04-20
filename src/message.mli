module Header : sig
  type t =
    { msg_id : string
    ; session : string
    ; username : string
    ; date : string option [@yojson.option]
    ; msg_type : string
    ; version : string
    }
  [@@deriving sexp_of, yojson]
end

module Execute_request_content : sig
  type t =
    { code : string
    ; silent : bool
    ; store_history : bool option
    ; user_expressions : (string * string) list
    ; allow_stdin : bool
    ; stop_on_error : bool
    }
  [@@deriving sexp, yojson]
end

module Execute_reply_content : sig
  type status =
    | Ok
    | Error
    | Aborted
  [@@deriving sexp, yojson]

  type t =
    { status : status
    ; execution_count : int
    ; user_expressions : (string * string) list option
    }
  [@@deriving sexp, yojson]
end

module Stream_content : sig
  type name =
    | Stdout
    | Stderr
  [@@deriving sexp, yojson]
end

module Status_content : sig
  type execution_state =
    | Busy
    | Idle
    | Starting
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

val status : Status_content.execution_state -> parent_header:Header.t -> t
val stream : Stream_content.name -> string -> parent_header:Header.t -> t
val kernel_info_reply : ids:string list -> parent_header:Header.t -> t
val read : _ Zmq_async.Socket.t -> key:string -> t Async.Deferred.t
val send : t -> _ Zmq_async.Socket.t -> key:string -> unit Async.Deferred.t
