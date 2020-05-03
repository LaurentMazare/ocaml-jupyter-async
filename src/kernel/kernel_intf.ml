open Core

type context =
  { write_display_data : Jupyter_async_message.Message.Display_data_content.t -> unit }

module type S = sig
  type t

  val create : context -> t
  val eval : t -> string -> unit Or_error.t
  val complete : t -> string -> int * (string * string) list
end
