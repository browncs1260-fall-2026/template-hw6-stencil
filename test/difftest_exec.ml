open Core
open Difftest

let () =
  let example_dir = "./examples" in
  ( match Sys_unix.file_exists example_dir with
  | `Yes ->
      ()
  | _ ->
      Core_unix.mkdir ~perm:0o777 example_dir ) ;
  let data_dir = "./data" in
      ( match Sys_unix.file_exists data_dir with
      | `Yes ->
          ()
      | _ ->
          Core_unix.mkdir ~perm:0o777 data_dir ) ;
      run_aggregate ~example_dir ~data_dir ;
  let difftest_results = run_difftests ~example_dir () in
  let inconsistent_tests =
    List.filter ~f:(Fn.non is_consistent) difftest_results
  in
  if List.is_empty difftest_results then
    Printf.printf
      "You have no tests - write some tests to check the correctness of your \
       implementation.\n"
  else if List.is_empty inconsistent_tests then
    Printf.printf "All %L tests passed!\n" (List.length difftest_results)
  else (
    Printf.printf
      "\n%L out of %L test failed. See inconsistent test(s) below: \n"
      (List.length inconsistent_tests)
      (List.length difftest_results) ;
    PrintBox_text.output stdout (difftest_box_output inconsistent_tests) ;
    exit 1 )

  