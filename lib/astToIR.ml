open Ir
open Ast

let fresh_temp =
  let count = ref 0 in
  fun () -> incr count; Printf.sprintf "t%d" !count

let rec expr_to_ir (e : expr) : ir_inst list * operand =
  match e with
  | Number n -> [], Imm n
  | ID x -> [], Var x
  | Binop (op, e1, e2) ->
      let code1, v1 = expr_to_ir e1 in
      let code2, v2 = expr_to_ir e2 in
      let res = Reg (fresh_temp ()) in
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | _ -> failwith "not implemented"
      in
      code1 @ code2 @ [Binop (op_str, res, v1, v2)], res
  | _ -> failwith "not implemented"
