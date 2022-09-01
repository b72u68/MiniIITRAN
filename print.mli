val string_of_pos : Lexing.position -> string
val string_of_typ : Ast.typ -> string

val get_bop : Ast.bop -> string * int * bool
val string_of_unop : Ast.unop -> string
val unop_lvl : int

(* Print an integer from the interpreter, formatted correctly for
 * the given type. *)
val pprint_as_type : Format.formatter -> int -> Ast.typ -> unit
val pprint_const : Format.formatter -> Ast.const -> unit
val pprint_exp : Format.formatter -> 'a Ast.exp -> unit
val pprint_string : Format.formatter -> string -> unit
val pprint_stmt_list : Format.formatter -> 'a Ast.stmt list -> unit
val pprint_stmt : Format.formatter -> 'a Ast.stmt -> unit
