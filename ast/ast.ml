open S_exp

type prim0 = ReadNum | Newline

type prim1 =
  | Add1
  | Sub1
  | IsZero
  | IsNum
  | Not
  | IsPair
  | IsEmpty
  | Left
  | Right
  | Print

type prim2 = Plus | Minus | Eq | Lt | Pair

type expr =
  | Num of int
  | Var of string
  | True
  | False
  | Nil
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Apply of string * expr
  | Call of string * expr list
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Do of expr list

type defn = {name: string; args: string list; rest: string option; body: expr}

type program = {defns: defn list; body: expr}

let is_defn defns name = List.exists (fun d -> d.name = name) defns

let get_defn defns name = List.find (fun d -> d.name = name) defns

(***** AST to S_exp *****)
let rec s_exp_of_expr = function
  | True ->
      Sym "true"
  | False ->
      Sym "false"
  | Nil ->
      Lst []
  | Num n ->
      Num n
  | Var var ->
      Sym var
  | Prim0 ReadNum ->
      Lst [Sym "read-num"]
  | Prim0 Newline ->
      Lst [Sym "newline"]
  | Prim1 (Add1, arg) ->
      Lst [Sym "add1"; s_exp_of_expr arg]
  | Prim1 (Sub1, arg) ->
      Lst [Sym "sub1"; s_exp_of_expr arg]
  | Prim1 (IsZero, arg) ->
      Lst [Sym "zero?"; s_exp_of_expr arg]
  | Prim1 (IsNum, arg) ->
      Lst [Sym "num?"; s_exp_of_expr arg]
  | Prim1 (Not, arg) ->
      Lst [Sym "not"; s_exp_of_expr arg]
  | Prim1 (IsPair, arg) ->
      Lst [Sym "pair?"; s_exp_of_expr arg]
  | Prim1 (Left, arg) ->
      Lst [Sym "left"; s_exp_of_expr arg]
  | Prim1 (Right, arg) ->
      Lst [Sym "right"; s_exp_of_expr arg]
  | Prim1 (IsEmpty, arg) ->
      Lst [Sym "empty?"; s_exp_of_expr arg]
  | Prim1 (Print, arg) ->
      Lst [Sym "print"; s_exp_of_expr arg]
  | Prim2 (Plus, arg1, arg2) ->
      Lst [Sym "+"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Minus, arg1, arg2) ->
      Lst [Sym "-"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Lt, arg1, arg2) ->
      Lst [Sym "<"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Eq, arg1, arg2) ->
      Lst [Sym "="; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Pair, arg1, arg2) ->
      Lst [Sym "pair"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | If (e1, e2, e3) ->
      Lst [Sym "if"; s_exp_of_expr e1; s_exp_of_expr e2; s_exp_of_expr e3]
  | Let (exps, eval_exp) ->
      let bindings =
        Lst
          (List.map
             (fun (e : string * expr) ->
               match e with v, expr -> Lst [Sym v; s_exp_of_expr expr] )
             exps )
      in
      Lst [Sym "let"; bindings; s_exp_of_expr eval_exp]
  | Do exps ->
      Lst (Sym "do" :: List.map s_exp_of_expr exps)
  | Apply (f, args_list) ->
      Lst [Sym "apply"; Sym f; s_exp_of_expr args_list]
  | Call (f, args) ->
      Lst (Sym f :: List.map s_exp_of_expr args)

let string_of_expr s = s |> s_exp_of_expr |> string_of_s_exp
