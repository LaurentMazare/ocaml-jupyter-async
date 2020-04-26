module F = Format
open Base

let verbose = ref true
let rewrite_lwt_async = ref true
let is_initialized = ref false

let set_topfind () =
  let log = ref [] in
  (Topfind.log := fun str -> log := str :: !log);
  Caml.Hashtbl.add
    Toploop.directive_table
    "topfind_log"
    (Toploop.Directive_none
       (fun () ->
         List.rev !log |> List.iter ~f:(Core.eprintf "%s\n%!");
         log := []));
  Caml.Hashtbl.add
    Toploop.directive_table
    "require"
    (Toploop.Directive_string
       (fun str ->
         let packages = String.split_on_chars str ~on:[ ' '; '\r'; '\n'; ','; '\t' ] in
         Findlib.package_deep_ancestors !Topfind.predicates packages |> Topfind.load));
  Topfind.add_predicates [ "byte"; "toploop" ];
  Topdirs.dir_directory (Findlib.package_directory "findlib")

let maybe_initialize () =
  if not !is_initialized
  then (
    Core.Sys.catch_break true;
    set_topfind ();
    Caml.Hashtbl.add
      Toploop.directive_table
      "verbose"
      (Toploop.Directive_bool (fun v -> verbose := v));
    Caml.Hashtbl.add
      Toploop.directive_table
      "rewrite_lwt_async"
      (Toploop.Directive_bool (fun v -> rewrite_lwt_async := v));
    is_initialized := true;
    Clflags.debug := true;
    Clflags.verbose := false;
    Warnings.parse_options false "-58";
    Location.formatter_for_warnings := F.err_formatter;
    Toploop.set_paths ();
    !Toploop.toplevel_startup_hook ();
    (* required for side-effect initialization in Topdirs *)
    Toploop.initialize_toplevel_env ();
    Sys.getenv "OCAML_TOPLEVEL_PATH" |> Option.iter ~f:Topdirs.dir_directory)

let exn_to_string exn ~code =
  let print_loc _ _report ppf (location : Location.t) =
    F.fprintf
      ppf
      "ocaml evaluation error on lines %d:%d to %d:%d\n"
      location.loc_start.pos_lnum
      location.loc_start.pos_cnum
      location.loc_end.pos_lnum
      location.loc_end.pos_cnum
  in
  let default_printer = Location.default_report_printer () in
  let report report_printer report ppf x =
    let location = report.Location.main.loc in
    F.pp_print_newline ppf ();
    let min_line_number = location.loc_start.pos_lnum - 5 in
    let max_line_number = location.loc_end.pos_lnum + 5 in
    String.rstrip code ~drop:(function
        | ' ' | '\r' | '\n' | '\t' -> true
        | _ -> false)
    |> String.split ~on:'\n'
    |> List.filter_mapi ~f:(fun lnum line ->
           let lnum = 1 + lnum in
           if min_line_number <= lnum && lnum <= max_line_number
           then (
             let marker =
               if location.loc_start.pos_lnum <= lnum && lnum <= location.loc_end.pos_lnum
               then ">"
               else " "
             in
             Some (Printf.sprintf "%s%3d: %s" marker lnum line))
           else None)
    |> String.concat ~sep:"\n"
    |> F.pp_print_string ppf;
    F.pp_print_newline ppf ();
    default_printer.Location.pp_main_txt report_printer report ppf x
  in
  let buffer = Buffer.create 256 in
  let formatter = F.formatter_of_buffer buffer in
  let report_printer () : Location.report_printer =
    { default_printer with
      Location.pp_main_loc = print_loc
    ; pp_submsg_loc = print_loc
    ; pp_main_txt = report
    }
  in
  Location.report_printer := report_printer;
  Location.report_exception formatter exn;
  Buffer.contents buffer

let toploop_eval str =
  try
    maybe_initialize ();
    let lexing = Lexing.from_string str in
    let phrases = !Toploop.parse_use_file lexing in
    List.iter phrases ~f:(fun phrase ->
        let phrase = Rewrite.apply_ppx phrase ~sourcefile:"jupyter-cell" in
        let phrase = if !rewrite_lwt_async then Rewrite.lwt_async phrase else phrase in
        let ok = Toploop.execute_phrase !verbose F.std_formatter phrase in
        ignore (ok : bool));
    F.pp_print_flush F.std_formatter ();
    Ok ()
  with
  | Caml.Sys.Break -> Or_error.error_string "interrupted by the user"
  | exn -> Or_error.error_string (exn_to_string exn ~code:str)
