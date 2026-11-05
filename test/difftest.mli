(** The output of a single execution, either interpreted or compiled. *)
type program_output = (string, unit) result

val string_of_program_output : (string, unit) result -> string
(** [string_of_program_output output] is a string representation of [output]. *)

(** A single test program, as well as all of its execution outputs. *)
type diff_result =
  { program_name: string
  ; expected_output: program_output option
  ; interp_output: program_output
  ; compiler_output: program_output }

val run_exit_status_and_output :
     ?stdin:string
  -> ?timeout:string
  -> string
  -> args:string list
  -> program_output
(** [run_exit_status_and_output path ~args] is the exit status and output of
 running the program at [path] with arguments [args]. *)

val interp :
  ?stdin:string -> interpreter_path:string -> string -> program_output
(** [interp ~interpreter_path path] is the output of running the program at
    [path] with the interpreter at [interpreter_path]. *)

val compile_and_run :
  ?stdin:string -> compiler_path:string -> string -> program_output
(** [compile_and_run ~compiler_path path] is the output of compiling and
    running the program at [path] with the compiler at [compiler_path]. *)

val test_program_names : example_dir:string -> string list
(** [test_program_names ~example_dir] is the list of all test programs in
    [example_dir]. *)

val get_expected_output : example_dir:string -> string -> program_output option
(** [get_expected_output ~example_dir name] is the expected output of the
    program named [name] in [example_dir]. *)

val get_input : example_dir:string -> string -> string option
(** [get_input ~example_dir name] is the input file of the program named
    [name] in [example_dir]. *)

val run_single_test :
     example_dir:string
  -> interpreter_path:string
  -> compiler_path:string
  -> string
  -> diff_result
(** [run_single_test ~example_dir ~interpreter_path ~compiler_path name] is
    the result of running the test program named [name] in [example_dir] with
    the interpreter at [interpreter_path] and the compiler at [compiler_path]. *)

val run_difftests :
     ?example_dir:string
  -> ?interpreter_path:string
  -> ?compiler_path:string
  -> unit
  -> diff_result list
(** [run_difftests ()] is the list of all test results, on default example and interpreter/compilers. *)

val difftest_box_output : diff_result list -> PrintBox.t
(** [difftest_box_output results] is a box representation of [results]. *)

val is_consistent : diff_result -> bool
(** [is_consistent result] is whether [result] is consistent, i.e. the
    interpreter and compiler outputs are the same. *)

val run_aggregate : example_dir:string -> data_dir:string -> unit

val agg_process :
  example_dir:string -> data_dir:string -> string list list -> unit