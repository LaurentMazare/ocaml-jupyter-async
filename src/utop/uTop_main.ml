(*
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)
(* +-----------------------------------------------------------------+
   | Toplevel expression rewriting                                   |
   +-----------------------------------------------------------------+ *)

let longident_lwt_main_run = Longident.Ldot (Longident.Lident "Lwt_main", "run")
let longident_async_thread_safe_block_on_async_exn =
  Longident.parse "Async.Thread_safe.block_on_async_exn"

#if OCAML_VERSION >= (4, 03, 0)
let nolabel = Asttypes.Nolabel
#else
let nolabel = ""
#endif

let with_loc loc str = {
  Location.txt = str;
  Location.loc = loc;
}

(* A rule for rewriting a toplevel expression. *)
type rewrite_rule = {
  type_to_rewrite : Longident.t;
  mutable path_to_rewrite : Path.t option;
  required_values : Longident.t list;
  (* Values that must exist and be persistent for the rule to apply. *)
  rewrite : Location.t -> Parsetree.expression -> Parsetree.expression;
  (* The rewrite function. *)
}

let rewrite_rules = [
  (* Rewrite Lwt.t expressions to Lwt_main.run <expr> *)
  {
    type_to_rewrite = Longident.parse "Lwt.t";
    path_to_rewrite = None;
    required_values = [longident_lwt_main_run];
    rewrite = (fun loc e ->
      let open Ast_helper in
      with_default_loc loc (fun () ->
        Exp.apply (Exp.ident (with_loc loc longident_lwt_main_run)) [(nolabel, e)]
      )
    );
  };

  (* Rewrite Async.Defered.t expressions to
     Async.Thread_safe.block_on_async_exn (fun () -> <expr>). *)
  {
    type_to_rewrite = Longident.parse "Async.Deferred.t";
    path_to_rewrite = None;
    required_values = [longident_async_thread_safe_block_on_async_exn];
    rewrite = (fun loc e ->
      let open Ast_helper in
      let punit = Pat.construct (with_loc loc (Longident.Lident "()")) None in
      with_default_loc loc (fun () ->
        Exp.apply
          (Exp.ident (with_loc loc longident_async_thread_safe_block_on_async_exn))
          [(nolabel, Exp.fun_ nolabel None punit e)]
      )
    );
  }
]

#if OCAML_VERSION >= (4, 10, 0)
let lookup_type longident env =
  Env.find_type_by_name longident env
#elif OCAML_VERSION >= (4, 04, 0)
let lookup_type longident env =
  let path = Env.lookup_type longident env in
  (path, Env.find_type path env)
#else
let lookup_type = Env.lookup_type
#endif

let rule_path rule =
  match rule.path_to_rewrite with
  | Some _ as x -> x
  | None ->
    try
      let env = !Toploop.toplevel_env in
      let path =
        match lookup_type rule.type_to_rewrite env with
        | path, { Types.type_kind     = Types.Type_abstract
                ; Types.type_private  = Asttypes.Public
                ; Types.type_manifest = Some ty
                ; _
                } -> begin
            match Ctype.expand_head env ty with
            | { Types.desc = Types.Tconstr (path, _, _); _ } -> path
            | _ -> path
          end
        | path, _ -> path
      in
      let opt = Some path in
      rule.path_to_rewrite <- opt;
      opt
    with _ ->
      None

(* Returns whether the given path is persistent. *)
let rec is_persistent_path = function
  | Path.Pident id -> Ident.persistent id
#if OCAML_VERSION >= (4, 08, 0)
  | Path.Pdot (p, _) -> is_persistent_path p
#else
  | Path.Pdot (p, _, _) -> is_persistent_path p
#endif
  | Path.Papply (_, p) -> is_persistent_path p

(* Check that the given long identifier is present in the environment
   and is persistent. *)
let is_persistent_in_env longident =
  let lookup_value=
#if OCAML_VERSION >= (4, 10, 0)
    Env.find_value_by_name
#else
    Env.lookup_value
#endif
  in
  try
    is_persistent_path (fst (lookup_value longident !Toploop.toplevel_env))
  with Not_found ->
    false

let rule_matches rule path =
  (match rule_path rule with
   | None -> false
   | Some path' -> Path.same path path') &&
  List.for_all is_persistent_in_env rule.required_values

(* Returns whether the argument is a toplevel expression. *)
let is_eval = function
  | { Parsetree.pstr_desc = Parsetree.Pstr_eval _; _ } -> true
  | _ -> false

(* Returns the rewrite rule associated to a type, if any. *)
let rule_of_type typ =
  match (Ctype.expand_head !Toploop.toplevel_env typ).Types.desc with
  | Types.Tconstr (path, _, _) -> begin
      try
        Some (List.find (fun rule -> rule_matches rule path) rewrite_rules)
      with _ ->
        None
    end
  | _ ->
    None

let rewrite_str_item pstr_item tstr_item =
  match pstr_item, tstr_item.Typedtree.str_desc with
    | ({ Parsetree.pstr_desc = Parsetree.Pstr_eval (e, _);
         Parsetree.pstr_loc = loc },
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ; _ }, _)) -> begin
      match rule_of_type typ with
        | Some rule ->
          { Parsetree.pstr_desc = Parsetree.Pstr_eval (rule.rewrite loc e, []);
            Parsetree.pstr_loc = loc }
        | None ->
          pstr_item
    end
    | _ ->
      pstr_item

let lwt_async phrase =
  match phrase with
    | Parsetree.Ptop_def pstr ->
      if List.exists is_eval pstr then
#if OCAML_VERSION >= (4, 08, 0)
        let tstr, _, _, _ = Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
#else
        let tstr, _, _ = Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
#endif
        Parsetree.Ptop_def (List.map2 rewrite_str_item pstr tstr.Typedtree.str_items)
      else
        phrase
    | Parsetree.Ptop_dir _ ->
      phrase

