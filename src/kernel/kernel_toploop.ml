type t = unit

let create () =
  Ocaml_toploop.maybe_initialize ();
  Complete.reset ();
  ()

let eval () code =
  let result = Ocaml_toploop.toploop_eval code in
  Complete.reset ();
  result

let complete () code = Complete.complete code
