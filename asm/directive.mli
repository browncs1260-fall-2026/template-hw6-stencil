(** Module for representing ASM directives and converting to ASM source code strings. *)

(** A processor register *)
type register = Rax | R8 | R9 | R10 | R11 | Rsp | Rdi | Rsi | Rdx | Rbp

(** An operand within an assembly directive *)
type operand = Reg of register | Imm of int | MemOffset of (operand * operand)

(** A single assembly directive *)
type directive =
  | Global of string
  | Extern of string
  | Section of string
  | Label of string
  | DqLabel of string
  | DqString of string
  | DqInt of int
  | Align of int
  | LeaLabel of (operand * string)
  | Mov of (operand * operand)
  | MovByte of (operand * operand)
  | Add of (operand * operand)
  | Sub of (operand * operand)
  | IDiv of operand
  | IMul of operand
  | Cqo
  | Shl of (operand * operand)
  | Shr of (operand * operand)
  | Sar of (operand * operand)
  | Cmp of (operand * operand)
  | And of (operand * operand)
  | Or of (operand * operand)
  | Setz of operand
  | Setl of operand
  | Jmp of string
  | Je of string
  | Jne of string
  | Jl of string
  | Jnl of string
  | Jg of string
  | Jng of string
  | ComputedJmp of operand
  | Ret
  | Push of operand
  | Pop of operand
  | Call of string
  | Comment of string

(** An entire ASM program, which consists of a list of directives *)
type asm_program = directive list

val string_of_register : ?byte:bool -> register -> string

val is_register : operand -> bool

val string_of_operand : ?byte:bool -> operand -> string

val label_name : bool -> string -> string

val string_of_directive : macos:bool -> directive -> string
