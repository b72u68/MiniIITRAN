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
(*%token <bool> LOGICAL*)
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
%left NEG NOT CCHAR CINT CLG

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
%type <Ast.const> const
%type <Ast.typ> typ

%%

const:
  | INT { CInt $1 }
  | CHAR { CChar $1 }
;

expr:
  (* constants *)
  | const { mk_exp (EConst $1) $loc }

  (* id *)
  | IDENT { mk_exp (EVar $1) $loc }

  (* binary operations *)
  | expr PLUS expr { mk_exp (EBinop (BAdd, $1, $3)) $loc }
  | expr MINUS expr { mk_exp (EBinop (BSub, $1, $3)) $loc }
  | expr TIMES expr { mk_exp (EBinop (BMul, $1, $3)) $loc }
  | expr DIV expr { mk_exp (EBinop (BDiv, $1, $3)) $loc }
  | expr AND expr { mk_exp (EBinop (BAnd, $1, $3)) $loc }
  | expr OR expr { mk_exp (EBinop (BOr, $1, $3)) $loc }
  | expr LT expr { mk_exp (EBinop (BLt, $1, $3)) $loc }
  | expr LE expr { mk_exp (EBinop (BLe, $1, $3)) $loc }
  | expr GT expr { mk_exp (EBinop (BGt, $1, $3)) $loc }
  | expr GE expr { mk_exp (EBinop (BGe, $1, $3)) $loc }
  | expr NE expr { mk_exp (EBinop (BNe, $1, $3)) $loc }
  | expr EQUAL expr { mk_exp (EBinop (BEq, $1, $3)) $loc }

  (* assign *)
  | expr ASSIGN expr { mk_exp (EAssign ($1, $3)) $loc }

  (* unary operations *)
  | NOT expr { mk_exp (EUnop (UNot, $2)) $loc }
  | NEG expr { mk_exp (EUnop (UNeg, $2)) $loc }
  | CINT expr { mk_exp (EUnop (UInt, $2)) $loc }
  | CCHAR expr { mk_exp (EUnop (UChar, $2)) $loc }
  | CLG expr { mk_exp (EUnop (ULog, $2)) $loc }

  (* grouping *)
  | LPAREN expr RPAREN { $2 }
;

closedstmt:
  | IF expr closedstmt ELSE closedstmt { mk_stmt $loc (SIf ($2, $3, Some $5)) }
  | expr { mk_stmt $loc (SExp $1) }
  | STOP { mk_stmt $loc SStop }
  | DO stmtlist END { mk_stmt $loc (SDo $2) }
  | WHILE expr closedstmt { mk_stmt $loc (SWhile ($2, $3)) }
;

openstmt:
  | IF expr stmt { mk_stmt $loc (SIf ($2, $3, None)) }
  | IF expr closedstmt ELSE openstmt { mk_stmt $loc (SIf ($2, $3, Some $5)) }
  | WHILE expr openstmt { mk_stmt $loc (SWhile ($2, $3)) }
;

stmt:
  | closedstmt { $1 }
  | openstmt { $1 }
;

typ:
  | TINT { TInteger }
  | TCHARACTER { TCharacter }
  | TLOGICAL { TLogical }
;

varlist:
  | IDENT { [$1] }
  | IDENT COMMA varlist { $1::$3 }
;

decl:
  | typ varlist { mk_stmt $loc (SDecl ($1, $2)) }
;

decllist:
  | { [] }
  | decl decllist { $1::$2 }
;

stmtlist:
  | { [] }
  | stmt stmtlist { $1::$2 }
;

prog:
| decllist stmtlist EOF { $1 @ $2 }
;
