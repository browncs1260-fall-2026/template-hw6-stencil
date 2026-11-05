open Core

type program_output = (string, unit) Result.t

let string_of_program_output = function
  | Ok output ->
      output
  | Error () ->
      "Error"

let wipe_tmp () =
  let tmpdir = "tmp" in
  match Sys_unix.file_exists tmpdir with
  | `Yes ->
      Sys_unix.readdir tmpdir
      |> Array.iter ~f:(fun x -> Sys_unix.remove (tmpdir ^ "/" ^ x))
  | _ ->
      Core_unix.mkdir ~perm:0o777 tmpdir

type diff_result =
  { program_name: string
  ; expected_output: program_output option
  ; interp_output: program_output
  ; compiler_output: program_output }

let run_exit_status_and_output ?stdin ?(timeout = "10s") cmd ~args :
    program_output =
  wipe_tmp () ;
  let open Shexp_process in
  (* Pipes (exit_code, output) to the eval *)
  match
    eval
      (pipe_both
         (err_to_out
            ( match stdin with
            | Some input ->
                pipe (run "echo" [input])
                  (run_exit_code "timeout" ([timeout; cmd] @ args))
            | None ->
                run_exit_code "timeout" ([timeout; cmd] @ args) ) )
         read_all )
  with
  | 0, output ->
      if String.is_substring output ~substring:"Error" then Error ()
      else Ok (String.strip output)
  | 124, _ ->
      Ok "Test timed out"
  | _, _ ->
      Error ()

let interp ?stdin ~interpreter_path filepath =
  run_exit_status_and_output ?stdin interpreter_path ~args:[filepath]

let compile_and_run ?stdin ~compiler_path filepath =
  run_exit_status_and_output ?stdin compiler_path
    ~args:["-r"; filepath; "examples_compiled"]

let test_program_names ~example_dir =
  Sys_unix.ls_dir example_dir
  |> List.filter ~f:(fun x ->
         match Filename.split_extension x with
         | _, Some ext ->
             List.mem ["lisp"; "mlb"] ext ~equal:String.equal
         | _ ->
             false )
  |> List.sort ~compare:String.compare

let get_expected_output ~example_dir program_name : program_output option =
  let out_file = program_name ^ ".out" in
  let err_file = program_name ^ ".err" in
  let out_file_exists = Sys_unix.file_exists (example_dir ^ "/" ^ out_file) in
  let err_file_exists = Sys_unix.file_exists (example_dir ^ "/" ^ err_file) in
  match (out_file_exists, err_file_exists) with
  | `Yes, _ ->
      Some
        (Ok (String.strip (In_channel.read_all (example_dir ^ "/" ^ out_file))))
  | _, `Yes ->
      Some (Error ())
  | _ ->
      None

let get_input ~example_dir program_name : string option =
  let input_file = program_name ^ ".in" in
  let input_file_exists =
    Sys_unix.file_exists (example_dir ^ "/" ^ input_file)
  in
  match input_file_exists with
  | `Yes ->
      Some (In_channel.read_all (example_dir ^ "/" ^ input_file))
  | _ ->
      None

let run_single_test ~example_dir ~interpreter_path ~compiler_path program_name =
  let program_name_no_ext = Filename.chop_extension program_name in
  let input = get_input ~example_dir program_name_no_ext in
  let interp_output =
    interp ?stdin:input ~interpreter_path (example_dir ^ "/" ^ program_name)
  in
  let compiler_output =
    compile_and_run ?stdin:input ~compiler_path
      (example_dir ^ "/" ^ program_name)
  in
  let expected_output = get_expected_output ~example_dir program_name_no_ext in
  { program_name= program_name_no_ext
  ; expected_output
  ; interp_output
  ; compiler_output }

let run_difftests ?(example_dir = "./examples")
    ?(interpreter_path = "./bin/interp.exe")
    ?(compiler_path = "./bin/compile.exe") () =
  test_program_names ~example_dir
  |> List.map ~f:(run_single_test ~example_dir ~interpreter_path ~compiler_path)

(** Displays results in a box*)
let difftest_box_output difftest_results =
  let result_rows_array =
    difftest_results
    |> List.map ~f:(fun res ->
           [| res.program_name
            ; ( match res.expected_output with
              | Some r ->
                  string_of_program_output r
              | None ->
                  "None Specified" )
            ; string_of_program_output res.interp_output
            ; string_of_program_output res.compiler_output |] )
  in
  PrintBox.grid
    ~pad:(PrintBox.pad' ~col:1 ~lines:0)
    (Array.of_list
       ( [ [|"Test"; "Expected"; "Interpreter"; "Compiler"|]
           |> Array.map ~f:(fun (x : string) ->
                  PrintBox.text_with_style PrintBox.Style.bold x ) ]
       @ ( result_rows_array
         |> List.map
              ~f:(Array.map ~f:(fun (x : string) -> PrintBox.sprintf "%s" x)) )
       ) )
  |> PrintBox.frame

let is_consistent difftest_result =
  let ( = ) = Result.equal String.equal (fun () () -> true) in
  match difftest_result with
  | { program_name= _
    ; expected_output= Some expected
    ; interp_output= interp
    ; compiler_output= compiler } ->
      expected = interp && interp = compiler
  | { program_name= _
    ; expected_output= None
    ; interp_output= interp
    ; compiler_output= compiler } ->
      interp = compiler

      let rec agg_process ~example_dir ~data_dir unpacked_list =
        match unpacked_list with
        | f :: r -> (
          match f with
          | [fname; content] ->
              let file =
                match fname with
                | "stdio" ->
                    example_dir ^ "/" ^ "stdio.in"
                | other ->
                    data_dir ^ "/" ^ other ^ ".txt"
              in
              print_endline "test" ;
              let oc = Out_channel.create file in
              Out_channel.fprintf oc "%s\n" content ;
              Out_channel.close oc ;
              agg_process ~example_dir ~data_dir r
          | [fname; input; output] ->
              let lisp_file = example_dir ^ "/" ^ fname ^ ".lisp" in
              let out =
                match output with
                | "" ->
                    example_dir ^ "/" ^ fname ^ ".err"
                | _ ->
                    example_dir ^ "/" ^ fname ^ ".out"
              in
              let l_oc = Out_channel.create lisp_file in
              Out_channel.fprintf l_oc "%s\n" input ;
              Out_channel.close l_oc ;
              let o_oc = Out_channel.create out in
              Out_channel.fprintf o_oc "%s" output ;
              Out_channel.close o_oc ;
              agg_process ~example_dir ~data_dir r
          | [fname; input; output; entries] ->
              let lisp_file = example_dir ^ "/" ^ fname ^ ".lisp" in
              let entry_file = example_dir ^ "/" ^ fname ^ ".in" in
              let out =
                match output with
                | "" ->
                    example_dir ^ "/" ^ fname ^ ".err"
                | _ ->
                    example_dir ^ "/" ^ fname ^ ".out"
              in
              let ins = Out_channel.create entry_file in 
              Out_channel.fprintf ins "%s\n" entries ; 
              Out_channel.close ins ;
              let l_oc = Out_channel.create lisp_file in
              Out_channel.fprintf l_oc "%s\n" input ;
              Out_channel.close l_oc ;
              let o_oc = Out_channel.create out in
              Out_channel.fprintf o_oc "%s" output ;
              Out_channel.close o_oc ;
              agg_process ~example_dir ~data_dir r
          | _ ->
              print_endline "check 2" ;
              agg_process ~example_dir ~data_dir r )
        | _ ->
            ()
      
      let run_aggregate ~example_dir ~data_dir =
        let agg_file = "index.in" in
        print_endline (example_dir ^ "/" ^ agg_file) ;
        let agg_file_exists = Sys_unix.file_exists (example_dir ^ "/" ^ agg_file) in
        match agg_file_exists with
        | `Yes ->
            let split_file =
              String.split_on_chars
                (In_channel.read_all (example_dir ^ "/" ^ agg_file))
                ~on:[',']
            in
            let _ =
              List.map split_file ~f:(fun s ->
                  List.map (String.split_on_chars s ~on:['|']) ~f:String.strip )
              |> List.sort ~compare:(fun x y -> List.length x - List.length y)
              |> agg_process ~example_dir ~data_dir
            in
            ()
        | _ ->
            print_endline "file doesn't exist"
      