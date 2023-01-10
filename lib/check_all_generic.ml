(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
 * Copyright (C) 2019 r2c
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
module Flag = Flag_parsing
module E = Error_code

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A driver for our different checkers:
 *  - unreachable statements (aka unused statement)
 *  - unused assignement (using dataflow liveness analysis)
 * 
 * todo:
 *  - use/def of global entities (functions, classes)
 *  - use/def of local variables
 *  - function/method call arity
 *  - type checker (e.g. wrong type of argument, expr is not a bool,
 *    use of array instead of scalar, etc)
 *  - record checker (fields)
 *  - protocol checker, statistical analysis a la Engler
 *  - ...
 *)

(*****************************************************************************)
(* Parse generic *)
(*****************************************************************************)

(* Put back in semgrep/.../parsing/pfff/?
 * We can't depend on semgrep/.../parsing/Parse_target.ml which
 * depends on too many semgrep libs.
 * coupling: in tests/test.ml and bin/Main.ml
*)
module Parse_generic = struct
let parse_program _file =
  failwith "TODO"
end
(*
module FT = File_type

let ast_generic_of_file file =
 let typ = File_type.file_type_of_file file in
 match typ with
 | FT.PL (FT.Web (FT.Js)) ->
    let cst = Parse_js.parse_program file in
    let ast = Ast_js_build.program cst in
    Js_to_generic.program ast
 | FT.PL (FT.Python) ->
    let ast = Parse_python.parse_program file in
    Resolve_python.resolve ast;
    Python_to_generic.program ast
 | _ -> failwith (spf "file type not supported for %s" file)

(* copy paste of code in pfff/main_test.ml *)
let dump_ast_generic file =
  let ast = ast_generic_of_file file in
  let v = Meta_ast.vof_any (Ast_generic.Pr ast) in
  let s = Ocaml.string_of_v v in
  pr2 s
*)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let check_file ?(verbose=true) ?(find_entity=None) ast =

 Common.save_excursion Flag_linter.verbose_checking verbose (fun() ->

  (* todo? some unsugaring? *)

  (* even if find_entity=None, check_and_annotate_program can find
   * interesting bugs on local variables. There will be false positives
   * but it's better than nothing.
   *)
  (* Check_variables_php.check_and_annotate_program find_entity ast; *)

  Check_cfg_generic.check_program ast;

(*
  (* not ready yet: Check_dfg_php.check_program ?find_entity ast; *)
  Check_micro_clones_php.check ast;
*)
  (* work only when have a find_entity; requires a global view of the code *)
  find_entity |> Option.iter (fun _find_entity ->
(*
    Check_functions_php.check_program find_entity ast;
    Check_classes_php.check_program   find_entity ast;
*)
    (* could have a Check_typedefs_php.check_program but hack will
     * already check the important things so no point doing redundant
     * checks.
     *)
    ()
  );
  ()
 )



(* TODO: should factorize code with check_graph_code.check, especially
 * the iteration over files, ranking and filtering of errors
 *)
let check ~show_progress ~rank ~filter ~r2c root files =

      let find_entity = None in
      
      files |> Console.progress ~show:show_progress (fun k -> 
        List.iter (fun file ->
          k();
          Error_code.try_with_exn_to_error file (fun () ->
            logger#info "processing: %s" file;
            let ast = 
              Common.save_excursion Flag.error_recovery false (fun () ->
              Common.save_excursion Flag.exn_when_lexical_error true (fun () ->
              Common.save_excursion Flag.show_parsing_error false (fun () ->
                Parse_generic.parse_program file 
             ))) in
            check_file ~find_entity ast;
          );
          if rank || r2c
          then ()
          else begin 
            let errs = 
              !E.g_errors 
              |> List.rev 
              |> List.filter (fun x -> 
                E.score_of_rank (E.rank_of_error x) >= filter
              ) 
              |>  E.filter_maybe_parse_and_fatal_errors in
            errs |> List.iter (fun err -> pr (E.string_of_error err));
            E.g_errors := []
          end
        )
    );

    if rank || r2c then begin
      let errs = 
        if rank
        then 
          !E.g_errors 
          |> List.map (fun x -> x, E.rank_of_error x)
          |> Common.sort_by_val_highfirst 
          |> List.map fst
          |> Common.take_safe 20 
        else !E.g_errors |> E.filter_maybe_parse_and_fatal_errors
      in
      if r2c 
      then 
        let errs = E.adjust_paths_relative_to_root root errs in
        pr (errs |> List.map E.string_of_error |> String.concat " ");
      else errs |> List.iter (fun err -> pr (E.string_of_error err))
    end
