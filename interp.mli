exception RuntimeError of Ast.loc * string

module Env = Varmap
type value = int
type env = (value * Ast.typ) Env.t

val empty_env : env
val val_of_const : Ast.const -> int * Ast.typ

val interp_exp : env -> 'a Ast.exp -> value * Ast.typ * env
val interp_stmt : env -> 'a Ast.stmt -> env
val interp_prog : 'a Ast.stmt list -> env
