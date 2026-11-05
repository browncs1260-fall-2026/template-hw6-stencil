(** Assembles asm_programs (directive lists) into binaries. 
    This handles linking, building, and cleaning up build files afterwards. *)

(** Assembly errors *)
type error = Expected of string | Unexpected of string

val build :
     directory:string
  -> runtime_text:string
  -> name:string
  -> instrs:Directive.asm_program
  -> string
(** [build directory runtime_text name instrs] assembles, links, and removes the object files to create a binary file with the given [name] in the given [directory]. *)

val eval :
     directory:string
  -> runtime_text:string
  -> name:string
  -> args:string list
  -> instrs:Directive.asm_program
  -> (string, error) result
(** [eval directory runtime_text name args instrs] assembles, links, and removes the object files to create a binary file with the given [name] in the given [directory], then runs it with the given [args], returning either the output or an error. *)

val eval_input :
     directory:string
  -> runtime_text:string
  -> name:string
  -> args:string list
  -> instrs:Directive.asm_program
  -> input:string
  -> (string, error) result
(** [eval_input directory runtime_text name args instrs input] assembles, links, and removes the object files to create a binary file with the given [name] in the given [directory], then runs it with the given [args] and [input], returning either the output or an error. *)
