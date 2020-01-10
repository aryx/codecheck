open Common
open OUnit
module E = Error_code

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests runner (and a few dumpers) *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
let verbose = ref false

let dump_ast = ref false

(* ran from _build/default/tests/ hence the '..'s below *)
let _tests_path = "../../../tests"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let test regexp =
  (* There is no reflection in OCaml so the unit test framework OUnit requires
   * us to explicitely build the test suites (which is not too bad).
   *)
  let tests =
    "all" >::: [
      Unit_linter.unittest ~ast_of_file:Parse_generic.parse_program;
    ]
  in
  let suite =
    if regexp = "all"
    then tests
    else
      let paths =
        OUnit.test_case_paths tests |> List.map OUnit.string_of_path in
      let keep = paths |> List.filter (fun path ->
        path =~ (".*" ^ regexp))
      in
      Common2.some (OUnit.test_filter keep tests)
  in

  let results = OUnit.run_test_tt ~verbose:!verbose suite in
  let has_an_error =
    results |> List.exists (function
    | OUnit.RSuccess _ | OUnit.RSkip _ | OUnit.RTodo _ -> false
    | OUnit.RFailure _ | OUnit.RError _ -> true
    )
  in
  if has_an_error
  then exit 1
  else exit 0

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

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

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let options = [
  "-verbose", Arg.Set verbose,
  " verbose mode";
  "-dump_ast", Arg.Set dump_ast,
  " <file> dump the generic Abstract Syntax Tree of a file";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let usage =
   Common.spf "Usage: %s [options] [regexp]> \nrun the unit tests matching the regexp\nOptions:"
      (Filename.basename Sys.argv.(0))

let main () =
  let args = ref [] in
  Arg.parse options (fun arg -> args := arg::!args) usage;

  (match List.rev !args with
  | [] -> test "all"
  | [file] when !dump_ast -> dump_ast_generic file
  | [x] -> test x
  | _::_::_ ->
    print_string "too many arguments\n";
    Arg.usage options usage;
  )

let _ = main ()
