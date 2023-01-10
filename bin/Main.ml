(* TODO: there are lots of cleanup to do on this file. It should be split in
 * many files
 *)
(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)
open Common

module Flag = Flag_parsing
module E = Error_code
module PI = Parse_info
module J = JSON

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Codecheck, a code checker supporting many languages.
 *)

(*---------------------------------------------------------------------------*)
(* Old scheck doc *)
(*---------------------------------------------------------------------------*)
(* A lint-like checker. https://github.com/facebook/pfff/wiki/Scheck
 * Right now there is support mainly for PHP, as well as for 
 * C, Java, and OCaml (via graph_code_checker.ml), and for Python
 * and Javascript (via the generic AST).
 * 
 * Note that scheck is mostly for generic bugs (which sometimes
 * requires global analysis). For API-specific bugs, use 'sgrep'.
 * todo: implement SgrepLint for scheck.
 * 
 * 'scheck' can leverage expensive global analysis results to find more 
 * bugs. It can use a light database (see pfff_db) or a graph_code database
 * (see codegraph).
 * 
 * todo:
 *  - https://medium.com/@Coder_HarryLee/the-way-static-analyzers-fight-against-false-positives-and-why-they-do-it-743de1f2a1bd#.x1ams12x6
 *  - more soft quality bugs, e.g. deadcode, dead parameter,
 *    passing whole structure when could just pass a subfield,
 *    wrong cohesion where a function should be in another file
 *    (e.g. when use 5 functions from another file and none of your file),
 *    or when a constant is used only in another file,
 *    etc
 * 
 * related:
 *  - https://atom.io/packages/linter
 *)

(*---------------------------------------------------------------------------*)
(* PHP specific doc *)
(*---------------------------------------------------------------------------*)
(*
 * By default 'scheck' performs only a local analysis of the file(s) passed
 * on the command line. It is thus quite fast while still detecting a few
 * important bugs like the use of undefined variables. 
 * It can also leverage more expensive global analysis to find more 
 * bugs. Doing so requires a PHP "code database", abstracted below
 * under the 'entity_finder' interface. 
 * 
 * Computing a code database is usually expensive to 
 * build (see pfff_db_heavy) and takes lots of space. 
 * Fortunately one can now build this database in memory, on the fly. 
 * Indeed, thanks to the include_require_php.ml analysis, we can now
 * build only the db for the files that matters, cutting significantly
 * the time to build the db (going down from 40 000 files to about 1000
 * files on average on facebook code). In a way it is similar
 * to what gcc does when it calls 'cpp' to get the full information for
 * a file. 
 * 
 * 'scheck' could also use the heavy database but this requires to have
 * the program linked with Berkeley DB, adding some dependencies to 
 * the user of the program. But because BDB is not very multi-user
 * friendly for now, and because Berkeley DB has been deprecated
 * in favor of the Prolog database (see main_codequery.ml) or 
 * graph_code database (see main_codegraph.ml), this option is not
 * supported anymore.
 * 
 * modes:
 *  - local analysis
 *  - perform global analysis "lazily" by building db on-the-fly
 *    of the relevant included files (configurable via a -depth_limit flag)
 *  - TODO leverage global analysis computed previously by pfff_db(light)
 *  - leverage global analysis computed previously by codegraph
 *  - nomore: global analysis computed by main_scheck_heavy.ml
 * 
 * current checks:
 *   - variable related (use of undeclared variable, unused variable, etc)
 *     with good handling of references false positives when have a code db
 *   - use/def of entities (e.g. use of undefined class/function/constant
 *     a la checkModule)
 *   - function call related (wrong number of arguments, bad keyword
 *     arguments, etc)
 *   - SEMI class related (use of undefined member, wrong number of arguments
 *     in method call, etc)
 *   - include/require and file related (e.g. including file that do not
 *     exist anymore); needs to pass an env
 *   - SEMI dead code (dead function in callgraph, DONE dead block in CFG,
 *     dead assignement in dataflow)
 *   - TODO type related
 *   - TODO resource related (open/close match)
 *   - TODO security related? via sgrep?
 *   - TODO require_strict() related (see facebook/.../main_linter.ml)
 * 
 * related: 
 *   - TODO lint_php.ml (small syntactic conventions, e.g. bad defines)
 *   - TODO check_code_php.ml (include/require stuff)
 *   - TODO check_module.ml (require_module() stuff), 
 *   - TODO main_linter.ml (require_strict() stuff), 
 *   - TODO main_checker.ml (flib-aware  checker),
 * 
 * todo: 
 *  - make it possible to take a db in parameter so
 *    for other functions, we can also get their prototype.
 *  - https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-2-coding-style-guide.md
 * 
 * The checks leverage also info about builtins, so when one calls preg_match(),
 * we know that this function takes things by reference which avoids
 * some false positives regarding the use of undeclared variable
 * for instance.
 * 
 * later: it could later also check javascript, CSS, sql, etc
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* less: infer from basename argv(0) ? *)
let lang = ref "python"

(* show only bugs with "rank" superior to this *)
let filter = ref 2
(* rank errors *)
let rank = ref false

(* see also the -report_xxx in error_code.ml *)

(* In strict mode, we can be more aggressive regarding scope like in JsLint. 
 * (This is a copy of a similar variable in Error_php.ml) 
*)
let strict = ref false

let auto_fix = ref false

(* running heavy analysis using the graph_code as the entity finder *)
let graph_code = ref (None: Common.filename option)

(* for codemap or layer_stat *)
let layer_file = ref (None: filename option)

(* display *)
let verbose = ref false
let show_progress = ref true
let r2c = ref false

let log_config_file = ref "log_config.json"

(* action mode *)
let action = ref ""

(*---------------------------------------------------------------------------*)
(* language specific flags *)
(*---------------------------------------------------------------------------*)

(* running the heavy analysis by processing the included files *)
let heavy = ref false
(* depth_limit is used to stop the expensive recursive includes process.
 * I put 5 because it's fast enough at depth 5, and 
 * I think it's good enough as it is probably bad for a file to use
 * something that is distant by more than 5 includes. 
 * 
 * todo: one issue is that some code like facebook uses special 
 *  require/include directives that include_require_php.ml is not aware of.
 *  Maybe we should have a unfacebookizer preprocessor that removes
 *  this sugar. The alternative right now is to copy most of the code
 *  in this file in facebook/qa_code/checker.ml :( and plug in the
 *  special include_require_php.ml hooks. 
 * Another alternative is to use the light_db or graph_code for the 
 * entity finder.
 *)
let depth_limit = ref (Some 5: int option)

let php_stdlib = 
  ref (Filename.concat Config_pfff.path_pfff_home "/data/php_stdlib")

(* old: main_scheck_heavy: let metapath = ref "/tmp/pfff_db" *)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let set_gc () =
(*
  if !Flag.debug_gc
  then Gc.set { (Gc.get()) with Gc.verbose = 0x01F };
*)
  (* see www.elehack.net/michael/blog/2010/06/ocaml-memory-tuning *)
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 2_000_000 };
  Gc.set { (Gc.get()) with Gc.space_overhead = 200 };
  ()

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  set_gc ();

  let xs = List.map Common.fullpath xs in
  (* less: could use Common2.find_common_root/common_prefix_of_files_or_dirs?*)
  let root =
    match xs with
    | [x] when Common2.is_directory x -> x
    | _ -> "/"
  in
  let lang = !lang in
  let files = Find_source.files_of_dir_or_files ~lang xs in

  match lang with
  | s when Lang.of_string_opt s <> None && !graph_code = None ->
(*---------------------------------------------------------------------------*)
(* AST generic checker *)
(*---------------------------------------------------------------------------*)
    Check_all_generic.check ~show_progress:!show_progress
        ~rank:!rank ~filter:!filter ~r2c:!r2c root files

(*---------------------------------------------------------------------------*)
(* Graphcode-based checker *)
(*---------------------------------------------------------------------------*)
  | "ocaml" | "ml" | "java2" | "c2" | "php2" | "clang2"  ->
     Check_graph_code.check 
        ~graph_code:!graph_code ~rank:!rank ~filter:!filter lang files

(*---------------------------------------------------------------------------*)
(* PHP checker *)
(*---------------------------------------------------------------------------*)

  | "php3" ->
      failwith "TODO: php checker"

  | _ -> failwith ("unsupported language: " ^ lang)
  

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* PHP type inference playground *)
(*---------------------------------------------------------------------------*)

let type_inference _file =
  raise Todo
(*

  let ast = Parse_php.parse_program file in

  (* PHP Intermediate Language *)
  try
    let pil = Pil_build.pil_of_program ast in

    (* todo: how bootstrap this ? need a bottom-up analysis but
     * we could first start with the types of the PHP builtins that
     * we already have (see builtins_php.mli in lang_php/analyze/).
     *)
    let env = () in

    (* works by side effect on the pil *)
    Type_inference_pil.infer_types env pil;

    (* simple pretty printer *)
    let s = Pretty_print_pil.string_of_program pil in
    pr s;

    (* internal representation pretty printer *)
    let s = Meta_pil.string_of_program
      ~config:{Meta_pil.show_types = true; show_tokens = false}
      pil
    in
    pr s;

  with exn ->
    pr2 "File contain constructions not supported by the PIL; bailing out";
    raise exn
*)

(* Dataflow analysis *)
let dflow _file_or_dir =
  failwith "TODO"
(*
  let file_or_dir = Common.fullpath file_or_dir in
  let files = Lib_parsing_php.find_source_files_of_dir_or_files [file_or_dir] in
  let dflow_of_func_def def =
    (try
       let flow = Controlflow_build_php.cfg_of_func def in
       let mapping = Dataflow_php.reaching_fixpoint flow in
       Dataflow_php.display_reaching_dflow flow mapping;
     with
     | Controlflow_build_php.Error err ->
       Controlflow_build_php.report_error err
     | Todo -> ()
     | Failure _ -> ()
    )
  in
  List.iter (fun file ->
    (try
       let ast = Parse_php.parse_program file in
       ast |> List.iter (function
       | Cst_php.FuncDef def ->
         dflow_of_func_def def
       | Cst_php.ClassDef def ->
         Cst_php.unbrace def.Cst_php.c_body |> List.iter
           (function
           | Cst_php.Method def -> dflow_of_func_def def
           | _ -> ())
       | _ -> ())
     with _ -> pr2 (spf "fail: %s" file)
    )) files
*)

(*---------------------------------------------------------------------------*)
(* Poor's man token-based Deadcode detector for C/C++/...  *)
(*---------------------------------------------------------------------------*)
module Ent = Entity_code
module Ast = Ast_fuzzy
module V = Lib_ast_fuzzy
let entities_of_ast ast =
  let res = ref [] in
  let visitor = V.mk_visitor { V.default_visitor with
    V.ktrees = (fun (k, _) xs ->
      (match xs with
      | Ast.Tok (s, _)::Ast.Parens _::Ast.Braces _::_res ->
          Common.push (s, Ent.Function) res;
      | _ ->  ()
      );
      k xs
    )
  }
  in
  visitor ast;
  !res

let test_index xs =
  let xs = List.map Common.fullpath xs in
  let hcnt, h = Check_graph_code.build_identifier_index "c++" xs in
(*
  hcnt |> Common.hash_to_list |> Common.sort_by_val_lowfirst 
  |> Common.take_safe 50 |> List.iter pr2_gen
*)
  hcnt |> Common.hash_to_list |> List.iter (fun (s, cnt) ->
    if cnt = 1 then
      let info = Hashtbl.find h s in
      let file = PI.file_of_info info in
      if file =~ ".*\\.c" 
      then begin
        (* pr2 (spf "found? %s in %s" s file); *)
        let (ast, _toks) = 
      try Parse_cpp.parse_fuzzy file
      with exn -> 
        pr2 (spf "PB fuzzy on %s (exn = %s)" file (Common.exn_to_s exn));
        [], []
    in
        let entities = entities_of_ast ast in
        (match Common2.assoc_opt s entities with
        | Some Ent.Function ->
            pr2 (spf "DEAD FUNCTION? %s in %s" s file)
        | _ -> ()
        )
      end
  )

(*****************************************************************************)
(* Regression testing *)
(*****************************************************************************)
open OUnit

let test () =
  let suite = "scheck" >:::[
(* TODO
      Unit_linter.unittest ~ast_of_file:Parse_generic.parse_program;
      (* Unit_checker_php.unittest *)
*)
  ]
  in
  OUnit.run_test_tt suite |> ignore;
  ()

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
     
let extra_actions () = [
  "-test", " run regression tests",
  Common.mk_action_0_arg test;

  "-test_type_inference", " <file>",
  Common.mk_action_1_arg type_inference;
  "-test_dflow", " <file/folder> run dataflow analysis",
  Common.mk_action_1_arg dflow;
  "-test_unprogress", " ",
  Common.mk_action_1_arg (fun file ->
    Common.cat file |> List.iter (fun s ->
      if s =~ ".*\\(flib/[^ ]+ CHECK: [^\n]+\\)"
      then pr (Common.matched1 s)
      else ()
    );
  );
  "-test_index", " <dirs>",
  Common.mk_action_n_arg test_index;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
 extra_actions() @
 (* Test_parsing_generic.actions() @ *)
 (* Test_analyze_generic.actions() @ *)
 []

let options () =
  [
    "-lang", Arg.Set_string lang, 
    (spf " <str> choose language (default = %s)" !lang);

    (* error filtering *)
    "-filter", Arg.Set_int filter,
    " <n> show only bugs whose importance >= n";
    "-rank", Arg.Set rank,
    " rank errors and display the 20 most important";

    (* error display *)
    "-emacs", Arg.Unit (fun () -> show_progress := false;), 
    " emacs friendly output";
    "-r2c", Arg.Unit (fun () ->
        r2c := true;
        show_progress := false),
    " use r2c platform error format for output";

    (* extra features *)
    "-with_graph_code", Arg.String (fun s -> graph_code := Some s), 
    " <file> use graph_code file for heavy analysis";
    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in pfff layer file";
    "-auto_fix", Arg.Set auto_fix,
    " try to auto fix the error\n";


    (* php specific *)
    "-php_stdlib", Arg.Set_string php_stdlib, 
    (spf " <dir> path to builtins (default = %s)" !php_stdlib);
    "-strict", Arg.Unit (fun () ->
        strict := true;
        Error_code.report_parse_errors := true;
        Error_code.report_fatal_errors := true;
    ),
    " emulate block scope instead of function scope";
    "-no_scrict", Arg.Clear strict, 
    " use function scope (default)";
    "-heavy", Arg.Set heavy,
    " process included files for heavy analysis";
    "-depth_limit", Arg.Int (fun i -> depth_limit := Some i), 
    " <int> limit the number of includes to process";
  ] @
  Error_code.options () @
  Common2.cmdline_flags_devel () @
  Common.options_of_actions action (all_actions()) @
  [
    "-verbose", Arg.Unit (fun () -> 
      verbose := true;
      (* Flag_analyze_php.verbose_entity_finder := true; *)
    ),
    " guess what";
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "scheck version: %s" Config_pfff.version);
      exit 0;
    ), " guess what";
  ]
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  let usage_msg =
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Scheck"
  in

  (*
   let handler = Easy_logging.(Handlers.make (CliErr Debug))
     (*
     match config.log_to_file with
     | None -> Easy_logging.(Handlers.make (CliErr Debug))
     | Some file -> Easy_logging.(Handlers.make (File (file, Debug)))
      *)
   in
   Logging.apply_to_all_loggers (fun logger -> logger#add_handler handler);
   (* Logging.(set_global_level Info); *)
*)
                                                                           
  if Sys.file_exists !log_config_file
  then begin
    Logging.load_config_file !log_config_file;
    logger#info "loaded %s" !log_config_file;
  end;

  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) ->
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) ->
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs ->
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] ->
        Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
