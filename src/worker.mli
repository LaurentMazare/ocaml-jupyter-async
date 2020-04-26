open Core
open Async

type t

val spawn : unit -> t
val execute : t -> code:string -> unit Or_error.t Deferred.t

val complete
  :  t
  -> code:string
  -> [ `ok of int * (string * string) list | `busy ] Deferred.t

val stdout_reader : t -> Reader.t
val stderr_reader : t -> Reader.t
val pid : t -> Pid.t
