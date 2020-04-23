open! Core

type t = { index : LibIndex.t }

let create () =
  let dirs =
    try
      let in_channel = Unix.open_process_in "ocamlc -where" in
      let dirs = In_channel.input_line in_channel |> Option.to_list in
      ignore (Unix.close_process_in in_channel : Unix.Exit_or_signal.t);
      LibIndex.Misc.unique_subdirs dirs
    with
    | Unix.Unix_error _ | Sys_error _ -> []
  in
  { index = LibIndex.load dirs }

let complete t str = LibIndex.complete t.index str |> List.map ~f:LibIndex.Print.path
