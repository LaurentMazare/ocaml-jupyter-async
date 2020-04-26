open Base

val maybe_initialize : unit -> unit
val toploop_eval : string -> verbose:bool -> unit Or_error.t
