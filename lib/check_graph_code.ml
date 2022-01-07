(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Generic checks using codegraph generated file (graph_code.marshall).
 *
 * This is mostly useful for global deadcode analysis for now.
 *)

(*---------------------------------------------------------------------------*)
(* PHP Entity finders *)
(*---------------------------------------------------------------------------*)

(* Build the database of information. Take the name of the file
 * we want to check and process all the files that are needed (included)
 * to check it, a la cpp.
 * 
 * Some checks needs to have a global view of the code, for instance
 * to know what are the sets of valid protected variable that can be used
 * in a child class.
 *)
let build_mem_db _file =
(*

  (* todo: could infer PHPROOT at least ? just look at
   * the include in the file and see where the files are.
   *)
  let env = 
    Env_php.mk_env (Common2.dirname file)
  in
  let root = "/" in (* todo ? *)

  let all_files = 
    Include_require_php.recursive_included_files_of_file 
      ~verbose:!verbose 
      ~depth_limit:!depth_limit
      env file
  in
  let builtin_files =
    Lib_parsing_php.find_php_files_of_dir_or_files [!php_stdlib]
  in
  Common.save_excursion Flag_analyze_php.verbose_database !verbose (fun()->
    Database_php_build.create_db
      ~db_support:(Database_php.Mem)
      ~phase:2 (* TODO ? *)
      ~files:(Some (builtin_files ++ all_files))
      ~verbose_stats:false
      ~annotate_variables_program:None
      (Database_php.prj_of_dir root) 
  )
*)
  raise Todo

let entity_finder_of_db file =
  let _db = build_mem_db file in
  raise Todo
(*
  Database_php_build.build_entity_finder db
*)

let entity_finder_of_graph_file _graph_file _root =
  raise Todo
(*
  let g = Graph_code.load graph_file in
  pr2 (spf "using %s for root" root);
  (* todo: the graph_code contains absolute path?? *)
  (Entity_php.entity_finder_of_graph_code g root, g)
*)

(* TODO, was in Main.ml for checking PHP code

    Flag_parsing.show_parsing_error := false;
    Flag_parsing.verbose_lexing := false;
    (* Error_php.strict := !strict; *)
    (* less: use a VCS.find... that is more general ?
     * infer PHP_ROOT? or take a --php_root?
     *)
    let _an_arg = List.hd xs |> Common2.relative_to_absolute in
    let root = 
      try (* Git.find_root_from_absolute_path an_arg  *) raise Todo
      with Not_found -> "/"
    in
    pr (spf "using %s for php_root" root);
    let env = Env_php.mk_env root in

    let (find_entity, graph_opt) =
      match () with
      | _ when !heavy ->
        Some (entity_finder_of_db (List.hd files)), None
      | _ when !graph_code <> None ->
        let (e, g) = 
          entity_finder_of_graph_file (Common2.some !graph_code) root in
        Some e, Some g
        (* old: main_scheck_heavy:
         * Database_php.with_db ~metapath:!metapath (fun db ->
         *  Database_php_build.build_entity_finder db
         *) 
      | _ -> None, None
    in
  
      files |> Console.progress ~show:!show_progress (fun k -> 
        List.iter (fun file ->
          k();
          try 
            pr2_dbg (spf "processing: %s" file);
            Check_all_php.check_file ~find_entity env file;
            (match graph_opt with
              | None -> ()
              | Some graph ->
                Check_classes_php.check_required_field graph file
            );
            let errs = 
              !Error_php._errors 
              |> List.rev 
              |> List.filter (fun x -> 
                Error_php.score_of_rank
                  (Error_php.rank_of_error_kind x.Error_php.typ) >= 
                  !filter
              )
            in
            if not !rank 
            then begin 
              errs |> List.iter (fun err -> pr (Error_php.string_of_error err));
              if !auto_fix then errs |> List.iter Auto_fix_php.try_auto_fix;
              Error_php._errors := []
            end
          with 
            | (Timeout | UnixExit _) as exn -> raise exn
            (*  | (Unix.Unix_error(_, "waitpid", "")) as exn -> raise exn *)
            | exn ->
              pr2 (spf "PB with %s, exn = %s" file (Common.exn_to_s exn));
              if !Common.debugger then raise exn
        ));

    if !rank then begin
      let errs = 
        !Error_php._errors 
        |> List.rev
        |> Error_php.rank_errors
        |> Common.take_safe 20 
      in
      errs |> List.iter (fun err -> pr (Error_php.string_of_error err));
      Error_php.show_10_most_recurring_unused_variable_names ();
      pr2 (spf "total errors = %d" (List.length !Error_php._errors));
      pr2 "";
      pr2 "";
    end;
    
    !layer_file |> Common.do_option (fun file ->
      (*  a layer needs readable paths, hence the root *)
      let root = Common2.common_prefix_of_files_or_dirs xs in
      Layer_checker_php.gen_layer ~root ~output:file !Error_php._errors
    );
*)

(*****************************************************************************)
(* Generic Helpers *)
(*****************************************************************************)

(* todo: could have more than 2 clients and still be dead
 * if was recursive function
 *)
let false_positive_detector hidentifier g errors =
  errors |> Common.exclude (fun err ->
    let fp = 
      match err.Error_code.typ with
      (* TODO: very specific to Semgrep, ugly. A skip_list would
       * be better but it does not seem to work
       *)
      | Error_code.Deadcode ((s, _)) when
         s =~ "^.*\\._[^\\.]+$" ||
         s =~ ".*\\.show_.*" ||
         s =~ ".*Tree_sitter.*__CST.*" ||
         s =~ ".*Tree_sitter.*__Boilerplate.*" ||
         err.Error_code.loc.Parse_info.file =~ ".*/pfff/.*" ||
         err.Error_code.loc.Parse_info.file =~ ".*_j.ml"
         ->
         true

      | Error_code.Deadcode ((s, Entity_code.Function) as n) ->
        let short = Graph_code.shortname_of_node n in
        let occurences = Hashtbl.find_all hidentifier short in
        let expected_minimum =
          if Graph_code.has_node (s, Entity_code.Prototype) g
          then 2 
          else 1
        in
        List.length occurences > expected_minimum
      | _ -> false
     in
     if fp
     then logger#debug "%s (FP deadcode?)" (Error_code.string_of_error err);
     fp
  )

(*****************************************************************************)
(* Language-specific Helpers *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* OCaml .cmt file issues *)
(*---------------------------------------------------------------------------*)
let file_with_wrong_loc file = 
  match Common2.dbe_of_filename_safe file with
  | Common2.Left (_, _, ("mly" | "mll" | "dyp")) -> true
  | _ when file =~ "TODO_NO_SOURCE_FOUND*" -> true
  | _ -> false

(*---------------------------------------------------------------------------*)
(* C++ false positive checker *)
(*---------------------------------------------------------------------------*)

(* mv types in a generic file in h_program-lang/? generalize this code
 * to use different lang, and so pass different tokenize and TIdent extractor.
 *)
module T = Parser_cpp
let build_identifier_index lang xs =
  let _root, files = 
    match xs with
    | [root] -> 
        root, Find_source.files_of_root ~lang root
    | _ ->
        let root = Common2.common_prefix_of_files_or_dirs xs in
        let files = 
          Find_source.files_of_dir_or_files ~lang xs in
        root, files
  in

  (* we use the Hashtbl.find_all property for this h *)
  let h = Hashtbl.create 101 in
  (* we don't here *)
  let hcnt = Hashtbl.create 101 in
  Flag_parsing.verbose_lexing := false;
  files |> List.iter (fun file ->
    let toks = Parse_cpp.tokens file in
       
    toks |> List.iter (fun tok ->
      match tok with
      | T.TIdent (s, info) ->
          if Hashtbl.mem hcnt s
          then 
            let cnt = Hashtbl.find hcnt s in
            if cnt > 10 then ()
            else begin
              Hashtbl.replace hcnt s (cnt + 1);
              Hashtbl.add h s info
            end
          else begin
            Hashtbl.add hcnt s 1;
            Hashtbl.add h s info
          end
      | _ -> ()
  ));
  hcnt, h

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: move those label arguments in a 'config' record *)
let check ~graph_code ~rank ~filter lang xs =
  let graph_file, _root =
    match xs, graph_code with
    | _,    Some file -> file, Filename.dirname file
    | [dir], _ -> Filename.concat dir Graph_code.default_filename, dir
    | x::xs, _ ->
        let root = Common2.common_prefix_of_files_or_dirs (x::xs) in
        Filename.concat root Graph_code.default_filename, root
    | [], _ -> failwith (spf "%s checker needs a graph file" lang);
  in
  if not (Sys.file_exists graph_file)
  then failwith (spf "%s checker needs a graph file" lang);

  let g = Graph_code.load graph_file in
  let errs = Graph_code_checker.check g in
  (* todo: make this more lazy? it's pretty slow *)
  let hidentifier =
    if lang = "clang2" (* not for "c"! graph_code_c is robust enough :) *)
    then build_identifier_index (if lang = "clang2" then "c++" else lang) xs |> snd
    else Hashtbl.create 0
  in

  let errs = 
    errs 
    |> false_positive_detector hidentifier g
    |> Error_code.adjust_errors
    |> List.filter (fun err -> (Error_code.score_of_error err) >= filter)
  in
  let errs = 
    if rank 
    then
      errs |> List.map (fun err -> err, Error_code.rank_of_error err)
      |> Common.sort_by_val_highfirst
      |> Common.take_safe 40
      |> List.map fst
    else errs
  in
  errs |> List.iter (fun err ->
    (* less: confront annotation and error kind *)
    if not (file_with_wrong_loc err.Error_code.loc.Parse_info.file) &&
       Error_code.annotation_at err.Error_code.loc <> None
    then logger#debug "%s (Skipping @)" (Error_code.string_of_error err)
    else pr (Error_code.string_of_error err)
  )
