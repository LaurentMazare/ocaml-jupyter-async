open! Core

let complete input =
  Jupyter_async_utop.UTop_complete.complete ~phrase_terminator:";;" ~input

let reset () = Jupyter_async_utop.UTop_complete.reset ()
