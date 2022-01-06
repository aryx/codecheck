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

(*****************************************************************************)
(* Generic Helpers *)
(*****************************************************************************)

(* todo: could have more than 2 clients and still be dead
 * if was recursive function
 *)
let false_positive_detector hidentifier g errors =
  errors |> Common.exclude (fun err ->
    match err.Error_code.typ with
    | Error_code.Deadcode ((s, Entity_code.Function) as n) ->
        let short = Graph_code.shortname_of_node n in
        let occurences = Hashtbl.find_all hidentifier short in
        let expected_minimum =
          if Graph_code.has_node (s, Entity_code.Prototype) g
          then 2 
          else 1
        in
        let fp = List.length occurences > expected_minimum in
        if fp
        then logger#debug "%s (FP deadcode?)" (Error_code.string_of_error err);
        fp
    | _ -> false
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
    else pr2 (Error_code.string_of_error err)
  )
