{
open Parser  (* token 类型在 parser.mly 中定义 *)
open Lexing

let reserved = [
  ("int", INT);
  ("void", VOID);
  ("if", IF);
  ("else", ELSE);
  ("while", WHILE);
  ("break", BREAK);
  ("continue", CONTINUE);
  ("return", RETURN);
]
}
let digit = ['0'-'9']
let nondigit = ['A'-'Z' 'a'-'z' '_']
let ident = (nondigit)(nondigit | digit)*
let number = '-'? ( '0' | ['1'-'9'] digit* )

rule read_token = parse
  | [' ' '\t' '\r' '\n']  { read_token lexbuf }  (* 忽略空白 *)

  (* 单行注释 *)
  | "//" [^ '\n']* '\n'   { read_token lexbuf }

  (* 多行注释 *)
  | "/*"                  { comment lexbuf }
 (* 关键词或ID *)
  | ident ->
    let id = lexeme lexbuf in
    (try List.assoc id reserved
     with Not_found -> ID id)

  (* 整数常量 *)
  | number as n -> NUMBER (int_of_string n)

  (* 运算符和符号 *)
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIV }
  | "%"  { MOD }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<"  { LT }
  | ">"  { GT }
  | "&&" { LAND }
  | "||" { LOR }
  | "="  { ASSIGN }
  | ";"  { SEMI }
  | ","  { COMMA }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | "!"  { NOT }

  | eof { EOF }

  | _ as c ->
      let pos = lexbuf.lex_curr_p in
      failwith (Printf.sprintf "Illegal character '%c' at line %d, column %d"
        c pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

and comment = parse
  | "*/" -> read_token lexbuf
  | eof -> failwith "Unterminated comment"
  | _    -> comment lexbuf
