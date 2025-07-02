open Compilerlib
open Ast

let parse_program (s : string) : func_def list =
  let lexbuf = Lexing.from_string s in
  try Parser.comp_unit Lexer.token lexbuf
  with Parsing.Parse_error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    let token = Lexing.lexeme lexbuf in
    Printf.eprintf "Syntax error at line %d, column %d: unexpected token '%s'\n"
      line col token;
    exit 1

let () =
  Printexc.record_backtrace true;
  let input =
    try really_input_string stdin (in_channel_length stdin)
    with End_of_file -> ""
  in
  let args = Array.to_list Sys.argv |> List.tl in
  let optimize = List.exists (( = ) "-opt") args in

  let ast = parse_program input in
  let ir = AstToIR.program_to_ir ast true in

  let asm = IrToAsm.compile_program ir in
  Printf.printf "%s\n" asm
