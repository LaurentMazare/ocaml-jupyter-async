open Core
open Async

type t

val spawn : unit -> t
val execute : t -> code:string -> unit Or_error.t Deferred.t
val stdout_reader : t -> Reader.t
val stderr_reader : t -> Reader.t
