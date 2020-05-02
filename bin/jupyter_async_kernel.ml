let () =
  Jupyter_async.Main.command (module Jupyter_async.Kernel_toploop) |> Core.Command.run
