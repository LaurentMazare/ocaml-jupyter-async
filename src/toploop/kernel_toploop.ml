open! Core

type t = unit

let create () =
  Ocaml_toploop.maybe_initialize ();
  Complete.reset ();
  Signal.Expert.handle Signal.int (fun _sigint ->
      if Async.Scheduler.is_running ()
      then Stdio.eprintf "received sigint, doing nothing as async is running\n%!"
      else raise Caml.Sys.Break)

let eval () code =
  let result = Ocaml_toploop.toploop_eval code in
  Complete.reset ();
  result

let complete () code = Complete.complete code
