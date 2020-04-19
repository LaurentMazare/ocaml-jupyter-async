module F = Format
open Base

let is_initialized = ref false

let maybe_initialize () =
  if not !is_initialized
  then (
    is_initialized := true;
    Clflags.debug := true;
    Clflags.verbose := false;
    Warnings.parse_options false "-58";
    Location.formatter_for_warnings := F.err_formatter;
    Toploop.set_paths ();
    !Toploop.toplevel_startup_hook ();
    (* required for side-effect initialization in Topdirs *)
    Toploop.initialize_toplevel_env ())

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

let toploop_eval str ~verbose =
  try
    maybe_initialize ();
    let lexing = Lexing.from_string str in
    let phrases = !Toploop.parse_use_file lexing in
    List.iter phrases ~f:(fun phrase ->
        let ok = Toploop.execute_phrase verbose F.std_formatter phrase in
        ignore (ok : bool));
    F.pp_print_flush F.std_formatter ();
    Ok ()
  with
  | exn -> Or_error.error_string (exn_to_string exn ~code:str)