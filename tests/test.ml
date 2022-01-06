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

(* ran from _build/default/tests/ hence the '..'s below *)
let _tests_path = "../../../tests"

(*****************************************************************************)
(* Helpers *)
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

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let options = [
  "-verbose", Arg.Set verbose,
  " verbose mode";
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
  | [x] -> test x
  | _::_::_ ->
    print_string "too many arguments\n";
    Arg.usage options usage;
  )

let _ = main ()
