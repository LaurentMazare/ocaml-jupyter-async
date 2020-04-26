module Header : sig
  type t [@@deriving sexp_of, yojson]
end

module Complete_request_content : sig
  type t =
    { code : string
    ; cursor_pos : int
    }
  [@@deriving sexp, yojson]
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

module Content : sig
  type t =
    | Kernel_info_request
    | Comm_info_request
    | Shutdown_request
    | Execute_request of Execute_request_content.t
    | Complete_request of Complete_request_content.t
    | Unsupported of { msg_type : string }
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val content : t -> Content.t
val header : t -> Header.t
val status : Status_content.execution_state -> parent_header:Header.t -> t
val stream : Stream_content.name -> string -> parent_header:Header.t -> t
val kernel_info_reply : t -> t
val comm_info_reply : t -> t
val shutdown_reply : t -> t

val execute_reply
  :  t
  -> status:Execute_reply_content.status
  -> execution_count:int
  -> user_expressions:(string * string) list option
  -> t

val complete_reply : t -> matches:string list -> cursor_start:int -> cursor_end:int -> t
val read : _ Zmq_async.Socket.t -> key:string -> t Async.Deferred.t
val send : t -> _ Zmq_async.Socket.t -> key:string -> unit Async.Deferred.t
