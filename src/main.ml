open Core
open Async

module Connection = struct
  type t =
    { control_port : int
    ; shell_port : int
    ; transport : string
    ; signature_scheme : string
    ; stdin_port : int
    ; hb_port : int
    ; ip : string
    ; iopub_port : int
    ; key : string
    }
  [@@deriving yojson, sexp]
end

let command =
  Command.async
    ~summary:"an ocaml kernel for jupyter"
    (let%map_open.Command connection_file =
       flag "-connection-file" (required string) ~doc:"a connection file in json format"
     in
     fun () ->
       let _connection = Yojson.Safe.from_file connection_file in
       Deferred.unit)
