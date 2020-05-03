open! Core

type t = unit

let create { Jupyter_async.Kernel_intf.write_display_data } =
  Ocaml_toploop.maybe_initialize ();
  Complete.reset ();
  Signal.Expert.handle Signal.int (fun _sigint ->
      if Async.Scheduler.is_running ()
      then Stdio.eprintf "received sigint, doing nothing as async is running\n%!"
      else raise Caml.Sys.Break);
  Ocaml_toploop.set_value "__write_display_data" write_display_data

let eval () code =
  let result = Ocaml_toploop.toploop_eval code in
  Complete.reset ();
  result

let complete () code = Complete.complete code
