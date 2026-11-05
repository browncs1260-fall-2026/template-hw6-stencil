open Core

type s_exp = [%import: Exp.t]

let show = Exp.show

let parse (s : string) =
  let buf = Lexing.from_string s in
  Parse.main Lex.token buf

let parse_many (s : string) =
  let buf = Lexing.from_string s in
  Parse.many Lex.token buf

let parse_file file =
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parse.main Lex.token lexbuf in
  In_channel.close inx ; ast

let parse_file_many file =
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  let ast = Parse.many Lex.token lexbuf in
  In_channel.close inx ; ast

let rec (string_of_s_exp : s_exp -> string) = function
  | Sym x ->
      x
  | Num n ->
      string_of_int n
  | Lst exps ->
      let exps = exps |> List.map ~f:string_of_s_exp in
      "(" ^ String.concat ~sep:" " exps ^ ")"
  | Chr c -> (
      "#\\"
      ^ match c with '\n' -> "newline" | ' ' -> "space" | _ -> String.make 1 c )
  | Str s ->
      "\"" ^ String.escaped s ^ "\""
  | Dots ->
      "..."
