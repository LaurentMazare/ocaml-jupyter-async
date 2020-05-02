open Core

module type S = sig
  type t

  val create : unit -> t
  val eval : t -> string -> unit Or_error.t
  val complete : t -> string -> int * (string * string) list
end
