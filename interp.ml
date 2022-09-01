(* IITRAN Interpreter *)
(* IIT CS 443, Fall 2022 *)
(* Project 0 *)

open Ast

exception RuntimeError of loc * string

module Env = Varmap

type value = int
type env = (value * typ) Env.t
let empty_env : env = Env.empty

let val_of_const =
  function CChar c -> (Char.code c, TCharacter)
         | CInt n -> (n, TInteger)
         | CLog false -> (0, TLogical)
         | CLog true -> (1, TLogical)

let to_log n = if n > 0 then 1 else 0
let log_of_bool = function true -> 1 | false -> 0

let do_bop b v1 v2 =
  match b with
  | BAdd -> (v1 + v2, TInteger)
  | BSub -> (v1 - v2, TInteger)
  | BMul -> (v1 * v2, TInteger)
  | BDiv -> (v1 / v2, TInteger)
  | BAnd -> (to_log (min v1 v2), TLogical)
  | BOr -> (to_log (max v1 v2), TLogical)
  | BGt -> (log_of_bool (v1 > v2), TLogical)
  | BGe -> (log_of_bool (v1 >= v2), TLogical)
  | BLt -> (log_of_bool (v1 < v2), TLogical)
  | BLe -> (log_of_bool (v1 <= v2), TLogical)
  | BNe -> (log_of_bool (v1 <> v2), TLogical)
  | BEq -> (log_of_bool (v1 = v2), TLogical)

let do_unop u v =
  match u with
  | UNeg -> (~- v, TInteger)
  | UNot -> (to_log (1 - v), TLogical)
  | UChar -> (v, TCharacter)
  | ULog -> (to_log v, TLogical)
  | UInt -> (v, TInteger)

exception Unimplemented

let rec interp_exp (env: env) (e: 'a exp) : int * typ * env =
  match e.edesc with
  | EAssign ({edesc = EVar var}, e) ->
          let (v, t, env) = interp_exp env e in
          let env' = (
              match Varmap.find_opt var env with
              | None -> Varmap.add var (v, t) env
              | Some _ -> Varmap.update var (fun _ -> Some (v, t)) env)
          in (v, t, env')
  | EAssign (_, _) ->
     raise (RuntimeError (e.eloc, "Left side of assignment not a variable"))
  | EBinop (op, e1, e2) ->
          let (v1, t1, env1) = interp_exp env e1 in
          let (v2, t2, env2) = interp_exp env1 e2 in
          let (v, t) = do_bop op v1 v2 in
          (v, t, env2)
  | EUnop (op, e) ->
          let (v, t, env') = interp_exp env e in
          let (v, t) = do_unop op v in
          (v, t, env')
  | EVar var ->
          (match Varmap.find_opt var env with
          | None -> raise (RuntimeError (e.eloc, Printf.sprintf "Undeclared variable %s" var))
          | Some (v, t) -> (v, t, env))
  | EConst c ->
          let (v, t) = val_of_const c in
          (v, t, env)

exception Stop of env

let rec interp_stmt (env: env) (s: 'a stmt) : env =
  match s.sdesc with
  | SDecl (t, vars) ->
     List.fold_left (fun env v -> Env.add v (0, t) env) env vars
  | SDo ss ->
     List.fold_left interp_stmt env ss
  | SExp e -> let (_, _, env) = interp_exp env e in env
  | SIf (e, s1, s2o) ->
     let (v, _, env) = interp_exp env e in
     if v > 0 then
       interp_stmt env s1
     else
       (match s2o with
        | Some s2 -> interp_stmt env s2
        | None -> env
       )
  | SWhile (e, s) ->
     let (v, _, env) = interp_exp env e in
     if v > 0 then
       let env = interp_stmt env s in
       interp_stmt env (mk_stmt s.sloc (SWhile (e, s)))
     else env
  | SStop -> raise (Stop env)

let rec interp_prog (p: 'a stmt list) : env =
  try
    List.fold_left interp_stmt empty_env p
  with Stop e -> e
