(* 引入 AST 和 IR 类型 *)
open Ast
open Ir

(* Map<String, operand> *)
module Env = Map.Make(String)

(* 
  TODO: function(参数传递策略), break, continue, return
*)

(* 临时寄存器生成器 *)
let temp_id = ref 0
let fresh_temp () =
  let id = !temp_id in
  incr temp_id;
  Reg ("t" ^ string_of_int id)

(* Label 生成器 *)
let label_id = ref 0
let fresh_label () =
  let id = !label_id in
  incr label_id;
  "L" ^ string_of_int id

(* 操作符映射 *)
let string_of_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<="
  | Greater -> ">" | Geq -> ">="
  | Land -> "&&" | Lor -> "||"

let string_of_unop = function
  | Not -> "!" | Plus -> "+" | Minus -> "-"

(* 表达式转换：返回目标寄存器和 IR 指令列表 *)
let rec expr_to_ir (env : operand Env.t) (e : expr) : operand * ir_inst list =
  match e with
  | Number n -> Imm n, []
  | ID name ->
      let operand = Env.find name env in
      operand, []
  | Binop (op, e1, e2) ->
      let lhs, code1 = expr_to_ir env e1 in
      let rhs, code2 = expr_to_ir env e2 in
      let res = fresh_temp () in
      res, code1 @ code2 @ [Binop (string_of_binop op, res, lhs, rhs)]
  | Unop (op, e1) ->
      let operand, code = expr_to_ir env e1 in
      let res = fresh_temp () in
      res, code @ [Unop (string_of_unop op, res, operand)]
  | Call (fname, args) ->
      let args_code, arg_operands =
        List.fold_right (fun arg (code_acc, op_acc) ->
          let op, code = expr_to_ir env arg in
          code @ code_acc, op :: op_acc
        ) args ([], [])
      in
      let ret = fresh_temp () in
      ret, args_code @ [Call (ret, fname, arg_operands)]

(* 语句转换：返回 IR 指令列表和更新后的环境 *)
let rec stmt_to_ir (env : operand Env.t) (s : stmt) : ir_inst list * operand Env.t =
  match s with
  | Empty -> [], env
  | ExprStmt e ->
      let _, code = expr_to_ir env e in
      code, env
  (* 赋初值 *)
  | Decl (name, Some e) ->
      let op, code = expr_to_ir env e in
      let var = Var name in
      code @ [Assign (var, op)], Env.add name var env
  | Decl (name, None) ->
      let var = Var name in
      [], Env.add name var env
  | Assign (name, e) ->
      let op, code = expr_to_ir env e in
      let var = Env.find name env in
      code @ [Assign (var, op)], env
  | Return None -> [Ret None], env
  | Return Some e ->
      let op, code = expr_to_ir env e in
      code @ [Ret (Some op)], env
  | If (cond, then_stmt, else_stmt) ->
      let cond_op, cond_code = expr_to_ir env cond in
      let then_code, _ = stmt_to_ir env then_stmt in
      let else_code, _ = match else_stmt with
        | Some s -> stmt_to_ir env s
        | None -> [], env
      in
      let l_then = fresh_label () in
      let l_else = fresh_label () in
      let l_end = fresh_label () in
      let code =
        cond_code @
        [IfGoto (cond_op, l_then); Goto l_else; Label l_then] @
        then_code @
        [Goto l_end; Label l_else] @
        else_code @
        [Label l_end]
      in
      code, env
  | While (cond, body_stmt) ->
      let l_cond = fresh_label () in
      let l_body = fresh_label () in
      let l_end = fresh_label () in
      let cond_op, cond_code = expr_to_ir env cond in
      let body_code, _ = stmt_to_ir env body_stmt in
      let code =
        [Goto l_cond; Label l_cond] @ cond_code @ [IfGoto (cond_op, l_body); Goto l_end; Label l_body] @
        body_code @ [Goto l_cond; Label l_end]
      in
      code, env
  | Block stmts ->
      (* TODO: 识别到 return 应该退出而非继续推导 *)
      List.fold_left (fun (acc_code, acc_env) stmt ->
        let code, new_env = stmt_to_ir acc_env stmt in
        acc_code @ code, new_env
      ) ([], env) stmts
  | Break | Continue ->
      (* TODO: 不好实现 *)
      failwith "Break/Continue not implemented yet"

(* 函数转换 *)
let func_to_ir (f : func_def) : ir_func =
  (* 函数参数作为初始的 env *)
  (* add <name, Var name> *)
  let env = List.fold_left (fun acc name -> Env.add name (Var name) acc) Env.empty f.params in
  (* 将 f.body 解析为 Block 并扔给 stmt *)
  let code, _ = stmt_to_ir env (Block f.body) in
  {
    name = f.func_name;
    args = f.params;
    body = code;
  }

(* 编译单元转换 *)
let program_to_ir (cu : comp_unit) : ir_program =
  List.map func_to_ir cu
