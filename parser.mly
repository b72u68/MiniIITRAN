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
%type <Ast.var list> varlist
%type <Ast.p_stmt> stmt
%type <Ast.p_stmt> closedstmt
%type <Ast.p_stmt> openstmt
%type <Ast.p_stmt> decl
%type <Ast.p_exp> expr
%type <Ast.p_exp> bexpr
%type <Ast.p_exp> uexpr
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
  | e=bexpr { e }

  (* unary expression *)
  | e=uexpr { e }

  (* assignment *)
  | e1=expr; ASSIGN; e2=expr { mk_exp (EAssign (e1, e2)) $loc }

  (* grouping *)
  | LPAREN; e=expr; RPAREN { e }

;

bexpr:
  | e1=expr; PLUS; e2=expr { mk_exp (EBinop (BAdd, e1, e2)) $loc }
  | e1=expr; MINUS; e2=expr { mk_exp (EBinop (BSub, e1, e2)) $loc }
  | e1=expr; TIMES; e2=expr { mk_exp (EBinop (BMul, e1, e2)) $loc }
  | e1=expr; DIV; e2=expr { mk_exp (EBinop (BDiv, e1, e2)) $loc }
  | e1=expr; AND; e2=expr { mk_exp (EBinop (BAnd, e1, e2)) $loc }
  | e1=expr; OR; e2=expr { mk_exp (EBinop (BOr, e1, e2)) $loc }
  | e1=expr; LT; e2=expr { mk_exp (EBinop (BLt, e1, e2)) $loc }
  | e1=expr; LE; e2=expr { mk_exp (EBinop (BLe, e1, e2)) $loc }
  | e1=expr; GT; e2=expr { mk_exp (EBinop (BGt, e1, e2)) $loc }
  | e1=expr; GE; e2=expr { mk_exp (EBinop (BGe, e1, e2)) $loc }
  | e1=expr; NE; e2=expr { mk_exp (EBinop (BNe, e1, e2)) $loc }
  | e1=expr; EQUAL; e2=expr { mk_exp (EBinop (BEq, e1, e2)) $loc }
;

uexpr:
  | NOT; e=expr { mk_exp (EUnop (UNot, e)) $loc }
  | NEG; e=expr { mk_exp (EUnop (UNeg, e)) $loc }
  | CINT; e=expr { mk_exp (EUnop (UInt, e)) $loc }
  | CCHAR; e=expr { mk_exp (EUnop (UChar, e)) $loc }
  | CLG; e=expr { mk_exp (EUnop (ULog, e)) $loc }
;

closedstmt:
  | IF; e=expr; s1=closedstmt; ELSE; s2=closedstmt { mk_stmt $loc (SIf (e, s1, Some s2)) }
  | e=expr { mk_stmt $loc (SExp e) }
  | STOP { mk_stmt $loc SStop }
  | DO; sl=stmtlist; END { mk_stmt $loc (SDo sl) }
  | WHILE; e=expr; s=closedstmt { mk_stmt $loc (SWhile (e, s)) }
;

openstmt:
  | IF; e=expr; s=stmt { mk_stmt $loc (SIf (e, s, None)) }
  | IF; e=expr; s1=closedstmt; ELSE; s2=openstmt { mk_stmt $loc (SIf (e, s1, Some s2)) }
  | WHILE; e=expr; s=openstmt { mk_stmt $loc (SWhile (e, s)) }
;

stmt:
  | s=closedstmt | s=openstmt { s }
;

typ:
  | TINT { TInteger }
  | TCHARACTER { TCharacter }
  | TLOGICAL { TLogical }
;

varlist:
  | i=IDENT { [i] }
  | i=IDENT; COMMA; vl=varlist { i::vl }
;

decl:
  | t=typ; vl=varlist { mk_stmt $loc (SDecl (t, vl)) }
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
