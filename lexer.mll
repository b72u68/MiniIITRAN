{

(* IITRAN Lexer *)
(* IIT CS 443, Fall 2022 *)
(* Stefan Muller *)
  open Parser
  exception Quit
  exception NeedMore
  exception SyntaxError

(* Taken from https://v2.ocaml.org/releases/4.14/htmlman/lexyacc.html#s%3Alexyacc-common-errors
and https://stackoverflow.com/questions/35068495/ocamllex-case-insenstitive*)
let keywords = Hashtbl.create 10
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
    [ "integer", TINT;
      "character", TCHARACTER;
      "logical", TLOGICAL;
      "and", AND;
      "or", OR;
      "not", NOT;
      "char", CCHAR;
      "int", CINT;
      "lg", CLG;
      "if", IF;
      "else", ELSE;
      "do", DO;
      "end", END;
      "while", WHILE;
      "stop", STOP
      ]
}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let ident = ['a'-'z' 'A'-'Z'] identchar*
let ws = [' ' '\t']

rule comment = parse
       | '\n' { Lexing.new_line lexbuf; token lexbuf }
       | _ { comment lexbuf}
and token = parse
       | ws { token lexbuf }
       | '\n' { Lexing.new_line lexbuf; token lexbuf }
       | '$' { comment lexbuf }
       | '-'? digit+ as n { INT (int_of_string n) }
       | '\'' ((_ # '\'') as c) '\'' { CHAR c }

       | ident as s { try Hashtbl.find keywords (String.lowercase_ascii s)
                      with Not_found -> IDENT (String.lowercase_ascii s) }
       | "<-" { ASSIGN }
       | "," { COMMA }
       | "+" { PLUS }
       | "-" { MINUS }
       | "~" { NEG }
       | "*" { TIMES }
       | "/" { DIV }
       | "<=" { LE }
       | "<" { LT }
       | ">=" { GE }
       | ">" { GT }
       | "#" { NE }
       | "=" { EQUAL }

       | "(" { LPAREN }
       | ")" { RPAREN }
       | eof { EOF }
