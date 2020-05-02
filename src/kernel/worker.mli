open Core
open Async

type display_data_output = Jupyter_async_message.Message.Display_data_content.t
type t

val spawn : (module Kernel_intf.S) -> t
val execute : t -> code:string -> unit Or_error.t Deferred.t

val complete
  :  t
  -> code:string
  -> [ `ok of int * (string * string) list | `busy ] Deferred.t

val stdout_reader : t -> Reader.t
val stderr_reader : t -> Reader.t
val display_data : t -> display_data_output Reader.Read_result.t Deferred.t
val pid : t -> Pid.t
