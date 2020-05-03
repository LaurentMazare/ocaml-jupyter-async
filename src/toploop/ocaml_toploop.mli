open Base

val maybe_initialize : unit -> unit
val toploop_eval : string -> unit Or_error.t
val set_value : string -> 'a -> unit
