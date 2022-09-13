%{
    (* IITRAN Parser *)
    (* IIT CS 443, Fall 2022 *)
    (* Project 0 *)
    (* My Dinh *)
    open Ast
    open Lexing
    open Lexer

    let syn_err = Ast.syn_err SyntaxError
%}

%token <int> INT
%token <char> CHAR
%token <string> IDENT
%token TINT TCHARACTER TLOGICAL
%token ASSIGN
%token PLUS MINUS TIMES DIV
%token AND OR
%token LT LE GT GE NE
%token EQUAL
%token NEG NOT CCHAR CINT CLG
%token IF ELSE
%token DO END
%token WHILE
%token STOP
%token LPAREN RPAREN COMMA
%token EOF

%right ASSIGN
%left OR
%left AND
%left LT LE GT GE NE EQUAL
%left PLUS MINUS
%left TIMES DIV
%nonassoc NEG NOT CCHAR CINT CLG

%start prog
%type <Ast.p_stmt list> prog
%type <Ast.p_stmt list> decllist
%type <Ast.p_stmt list> stmtlist
%type <Ast.p_stmt> stmt
%type <Ast.p_stmt> closedstmt
%type <unit Ast.stmt_> closedstmt_
%type <Ast.p_stmt> openstmt
%type <unit Ast.stmt_> openstmt_
%type <Ast.p_stmt> decl
%type <Ast.var list> separated_nonempty_list(COMMA, IDENT)
%type <Ast.p_exp> expr
%type <unit Ast.exp_> bexpr_
%type <unit Ast.exp_> uexpr_
%type <Ast.const> const
%type <Ast.typ> typ

%%

const:
  | n=INT { CInt n }
  | c=CHAR { CChar c }
;

expr:
  (* constants *)
  | c=const { mk_exp (EConst c) $loc }

  (* id *)
  | i=IDENT { mk_exp (EVar i) $loc }

  (* binary expression *)
  | e=bexpr_ { mk_exp e $loc }

  (* unary expression *)
  | e=uexpr_ { mk_exp e $loc }

  (* assignment *)
  | e1=expr; ASSIGN; e2=expr { mk_exp (EAssign (e1, e2)) $loc }

  (* grouping *)
  | LPAREN; e=expr; RPAREN { e }

;

bexpr_:
  | e1=expr; PLUS; e2=expr { EBinop (BAdd, e1, e2) }
  | e1=expr; MINUS; e2=expr { EBinop (BSub, e1, e2) }
  | e1=expr; TIMES; e2=expr { EBinop (BMul, e1, e2) }
  | e1=expr; DIV; e2=expr { EBinop (BDiv, e1, e2) }
  | e1=expr; AND; e2=expr { EBinop (BAnd, e1, e2) }
  | e1=expr; OR; e2=expr { EBinop (BOr, e1, e2) }
  | e1=expr; LT; e2=expr { EBinop (BLt, e1, e2) }
  | e1=expr; LE; e2=expr { EBinop (BLe, e1, e2) }
  | e1=expr; GT; e2=expr { EBinop (BGt, e1, e2) }
  | e1=expr; GE; e2=expr { EBinop (BGe, e1, e2) }
  | e1=expr; NE; e2=expr { EBinop (BNe, e1, e2) }
  | e1=expr; EQUAL; e2=expr { EBinop (BEq, e1, e2) }
;

uexpr_:
  | NOT; e=expr { EUnop (UNot, e) }
  | NEG; e=expr { EUnop (UNeg, e) }
  | CINT; e=expr { EUnop (UInt, e) }
  | CCHAR; e=expr { EUnop (UChar, e) }
  | CLG; e=expr { EUnop (ULog, e) }
;

closedstmt_:
  | IF; e=expr; s1=closedstmt; ELSE; s2=closedstmt { SIf (e, s1, Some s2) }
  | e=expr { SExp e }
  | STOP { SStop }
  | DO; sl=stmtlist; END { SDo sl }
  | WHILE; e=expr; s=closedstmt { SWhile (e, s) }
;

closedstmt:
  | closedstmt_ { mk_stmt $loc $1 }
;

openstmt_:
  | IF; e=expr; s=stmt { SIf (e, s, None) }
  | IF; e=expr; s1=closedstmt; ELSE; s2=openstmt { SIf (e, s1, Some s2) }
  | WHILE; e=expr; s=openstmt { SWhile (e, s) }
;

openstmt:
  | openstmt_ { mk_stmt $loc $1 }
;

stmt:
  | s=closedstmt | s=openstmt { s }
;

typ:
  | TINT { TInteger }
  | TCHARACTER { TCharacter }
  | TLOGICAL { TLogical }
;

decl:
  | t=typ; vl=separated_nonempty_list(COMMA, IDENT) { mk_stmt $loc (SDecl (t, vl)) }
;

decllist:
  | { [] }
  | d=decl; dl=decllist { d::dl }
;

stmtlist:
  | { [] }
  | s=stmt; sl=stmtlist { s::sl }
;

prog:
  | dl=decllist; sl=stmtlist; EOF { dl @ sl }
;
