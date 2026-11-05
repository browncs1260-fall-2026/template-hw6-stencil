open Core
open Csci1260

let command =
  Command.basic ~summary:"Interpret the given file"
    Command.Let_syntax.(
      let%map_open filename =
        anon (maybe ("filename" %: Filename_unix.arg_type))
      and expression =
        flag "-e" (optional string) ~doc:"expression to evaluate"
      in
      fun () ->
        try
          match (filename, expression) with
          | Some f, _ ->
              S_exp.parse_file_many f |> Interp.interp ;
              print_endline ""
          | _, Some e ->
              S_exp.parse_many e |> Interp.interp ;
              print_endline ""
          | _ ->
              Printf.eprintf
                "Error: must specify either an expression to evaluate or a file\n" ;
              exit 1
        with e ->
          Printf.eprintf "Error: %s\n" (Exn.to_string e) ;
          exit 1 )

let () = Command_unix.run ~version:"1.0" command
