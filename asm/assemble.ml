open Core

type error = [%import: Assemble.error]

let run cmd ~args =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (run cmd args |- read_all)

let run_exit_code cmd ~args =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (run_exit_code cmd args |+ read_all)

let run_exit_code_input cmd ~args ~input =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (echo input |- run_exit_code cmd args |+ read_all)

let asm_name ~directory ~name = Printf.sprintf "%s/%s.s" directory name

let object_name ~directory ~name = Printf.sprintf "%s/%s.o" directory name

let binary_name ~directory ~name = Printf.sprintf "%s/%s.exe" directory name

let macos () = run "uname" ~args:["-s"] |> String.strip |> String.equal "Darwin"

let asm_to_file ~instrs ~asm_file =
  let text =
    instrs
    |> List.map ~f:(Directive.string_of_directive ~macos:(macos ()))
    |> String.concat ~sep:"\n"
  in
  let file =
    Core_unix.openfile asm_file
      ~mode:[Core_unix.O_WRONLY; Core_unix.O_CREAT; Core_unix.O_TRUNC]
      ~perm:0o666
  in
  Core_unix.write_substring file ~buf:text ~pos:0 ~len:(String.length text)
  |> fun _ -> Core_unix.close file

let assemble ~asm_file ~object_file =
  let format = if macos () then "macho64" else "elf64" in
  run "nasm" ~args:[asm_file; "-o"; object_file; "-f"; format] |> ignore

let copy_runtime ~runtime_file ~runtime_text =
  let file =
    Core_unix.openfile runtime_file
      ~mode:[Core_unix.O_WRONLY; Core_unix.O_CREAT; Core_unix.O_TRUNC]
      ~perm:0o666
  in
  Core_unix.write_substring file ~buf:runtime_text ~pos:0
    ~len:(String.length runtime_text)
  |> fun _ -> Core_unix.close file

let link ~object_file ~runtime_file ~binary_file =
  let disable_pie = if macos () then "-Wl,-no_pie" else "-no-pie" in
  run "gcc"
    ~args:
      [ disable_pie
      ; object_file
      ; "-g"
      ; runtime_file
      ; "-o"
      ; binary_file
      ; "-z"
      ; "execstack" ]
  |> ignore

let remove_object_files ~object_file ~runtime_file =
  run "rm" ~args:[object_file; runtime_file] |> ignore

let build ~directory ~runtime_text ~name ~instrs =
  let _ =
    try Core_unix.mkdir directory ~perm:0o777
    with Core_unix.Unix_error _ -> ()
  in
  let asm_file = asm_name ~directory ~name in
  let object_file = object_name ~directory ~name in
  let runtime_file = object_name ~directory ~name:"runtime" in
  let binary_file = binary_name ~directory ~name in
  asm_to_file ~instrs ~asm_file ;
  assemble ~asm_file ~object_file ;
  copy_runtime ~runtime_file ~runtime_text ;
  link ~object_file ~runtime_file ~binary_file ;
  remove_object_files ~object_file ~runtime_file ;
  binary_file

let eval ~directory ~runtime_text ~name ~args ~instrs =
  let exit =
    try Ok (run_exit_code (build ~directory ~runtime_text ~name ~instrs) ~args)
    with e -> Error (Unexpected (Exn.to_string e))
  in
  Result.bind exit ~f:(function
    | 0, output ->
        Ok output
    | 1, output ->
        Error (Expected output)
    | code, output ->
        Error (Unexpected (Printf.sprintf "Exited with code %d: %s" code output)) )

let eval_input ~directory ~runtime_text ~name ~args ~instrs ~input =
  let exit =
    try
      Ok
        (run_exit_code_input
           (build ~directory ~runtime_text ~name ~instrs)
           ~args ~input )
    with e -> Error (Unexpected (Exn.to_string e))
  in
  Result.bind exit ~f:(function
    | 0, output ->
        Ok output
    | 1, output ->
        Error (Expected output)
    | code, output ->
        Error (Unexpected (Printf.sprintf "Exited with code %d: %s" code output)) )
