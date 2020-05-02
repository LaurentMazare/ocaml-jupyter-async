let apply_ppx ~sourcefile phrase =
  match phrase with
  | Parsetree.Ptop_def ptop_def ->
    let _ = sourcefile in
    let ptop_def =
      Pparse.apply_rewriters_str ptop_def ~restore:true ~tool_name:"ocaml"
#if OCAML_VERSION < (4,09,0)
      |> Pparse.ImplementationHooks.apply_hooks { Misc.sourcefile }
#endif
    in
    Parsetree.Ptop_def ptop_def
  | Ptop_dir _ as phrase -> phrase

let lwt_async = Jupyter_async_utop.UTop_main.lwt_async
