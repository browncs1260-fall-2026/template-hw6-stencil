open Core

(* Type definitions have been moved to the directive.mli file.
   Refer to the interface file for definitions. *)
type register = [%import: Directive.register]

type operand = [%import: Directive.operand]

type directive = [%import: Directive.directive]

type asm_program = [%import: Directive.asm_program]

let string_of_register ?(byte = false) (reg : register) =
  match (reg, byte) with
  | Rax, false ->
      "rax"
  | Rax, true ->
      "al"
  | R8, false ->
      "r8"
  | R8, true ->
      "r8b"
  | R9, false ->
      "r9"
  | R9, true ->
      "r9b"
  | R10, false ->
      "r10"
  | R10, true ->
      "r10b"
  | R11, false ->
      "r11"
  | R11, true ->
      "r11b"
  | Rsp, _ ->
      "rsp"
  | Rdi, _ ->
      "rdi"
  | Rsi, _ ->
      "rsi"
  | Rdx, _ ->
      "rdx"
  | Rbp, _ ->
      "rbp"

let is_register o = match o with Reg _ -> true | _ -> false

let rec string_of_operand ?(byte = false) = function
  | Reg r ->
      string_of_register ~byte r
  | Imm i ->
      string_of_int i
  | MemOffset (o1, o2) ->
      if byte then
        Printf.sprintf "BYTE [%s + %s]"
          (string_of_operand ~byte:false o1)
          (string_of_operand ~byte:false o2)
      else
        Printf.sprintf "QWORD [%s + %s]"
          (string_of_operand ~byte:false o1)
          (string_of_operand ~byte:false o2)

let label_name (macos : bool) (label : string) : string =
  Printf.sprintf (if macos then "_%s" else "%s") label

let string_of_directive ~macos = function
  (* frontmatter *)
  | Global l ->
      Printf.sprintf
        (if macos then "default rel\nglobal _%s" else "global %s")
        l
  | Extern l ->
      Printf.sprintf "extern %s" (label_name macos l)
  | Section l ->
      Printf.sprintf "\tsection .%s" l
  (* labels *)
  | Label l ->
      Printf.sprintf "%s:" (label_name macos l)
  (* data *)
  | DqLabel l ->
      Printf.sprintf "\tdq %s" (label_name macos l)
  | DqString l ->
      Printf.sprintf "\tdq `%s`, 0" (String.escaped l)
  | DqInt i ->
      Printf.sprintf "\tdq %d" i
  | Align i ->
      Printf.sprintf "align %d" i
  (* instructions *)
  | Mov (dest, src) ->
      Printf.sprintf "\tmov %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | MovByte (dest, src) ->
      Printf.sprintf "\tmov %s, %s"
        (string_of_operand ~byte:true dest)
        (string_of_operand ~byte:true src)
  | Add (dest, src) ->
      Printf.sprintf "\tadd %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Sub (dest, src) ->
      Printf.sprintf "\tsub %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | IDiv src ->
      Printf.sprintf "\tidiv QWORD %s" (string_of_operand src)
  | IMul src ->
      Printf.sprintf "\timul QWORD %s" (string_of_operand src)
  | Cqo ->
      Printf.sprintf "\tcqo"
  | Shl (dest, src) ->
      Printf.sprintf "\tshl %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Shr (dest, src) ->
      Printf.sprintf "\tshr %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Sar (dest, src) ->
      Printf.sprintf "\tsar %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Cmp (dest, src) ->
      Printf.sprintf "\tcmp %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | And (dest, src) ->
      Printf.sprintf "\tand %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Or (dest, src) ->
      Printf.sprintf "\tor %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Setz dest ->
      Printf.sprintf "\tsete %s" (string_of_operand ~byte:true dest)
  | Setl dest ->
      Printf.sprintf "\tsetl %s" (string_of_operand ~byte:true dest)
  | LeaLabel (dest, label) ->
      Printf.sprintf "\tlea %s, [%s]" (string_of_operand dest)
        (label_name macos label)
  | Jmp dest ->
      Printf.sprintf "\tjmp %s" (label_name macos dest)
  | Je dest ->
      Printf.sprintf "\tje %s" (label_name macos dest)
  | Jne dest ->
      Printf.sprintf "\tjne %s" (label_name macos dest)
  | Jl dest ->
      Printf.sprintf "\tjl %s" (label_name macos dest)
  | Jnl dest ->
      Printf.sprintf "\tjnl %s" (label_name macos dest)
  | Jg dest ->
      Printf.sprintf "\tjg %s" (label_name macos dest)
  | Jng dest ->
      Printf.sprintf "\tjng %s" (label_name macos dest)
  | ComputedJmp dest ->
      Printf.sprintf "\tjmp %s" (string_of_operand dest)
  | Push o ->
      Printf.sprintf "\tpush %s" (string_of_operand o)
  | Pop o ->
      Printf.sprintf "\tpop %s" (string_of_operand o)
  | Call dest ->
      Printf.sprintf "\tcall %s" (label_name macos dest)
  | Ret ->
      "\tret"
  | Comment s ->
      Printf.sprintf "; %s" s
