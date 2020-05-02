let () =
  Jupyter_async.Main.command (module Jupyter_async_toploop.Kernel_toploop)
  |> Core.Command.run
