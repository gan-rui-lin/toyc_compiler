(*astToIR.ml*)
(* 引入 AST 和 IR 类型 *)
open Ast
open Ir

(* Map<String, operand> *)
module Env = Map.Make (String)

(* 翻译上下文：包含当前 env 及循环标签，用于实现 break/continue *)
type context = {
  mutable env : operand Env.t;
  break_lbl : string option; (* break 跳转目标 *)
  continue_lbl : string option; (* continue 跳转目标 *)
}

module LabelMap = Map.Make (String)

(* 临时寄存器与标签生成器 *)
let temp_id = ref 0

let fresh_temp () =
  let id = !temp_id in
  incr temp_id;
  Reg ("t" ^ string_of_int id)

let label_id = ref 0
let ir_label_id = ref 0

let fresh_label () =
  let id = !label_id in
  incr label_id;
  "L" ^ string_of_int id

let fresh_IRlabel (label_map : string LabelMap.t) (l : param) :
    string * string LabelMap.t =
  (* 先查找 l 是否已经分配了 label，如果有直接返回，否则分配新 label，并写回 map *)
  match LabelMap.find_opt l label_map with
  | Some lbl -> (lbl, label_map)
  | None ->
      let id = !ir_label_id in
      incr ir_label_id;
      let lbl = "LABEL" ^ string_of_int id in
      let label_map' = LabelMap.add l lbl label_map in
      (lbl, label_map')

(* 操作符到字符串映射 *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Land -> "&&"
  | Lor -> "||"

let string_of_unop = function Not -> "!" | Plus -> "+" | Minus -> "-"

(* stmt_to_res 用于处理 return 终止：Normal/Returned 两种结果 *)
type stmt_res = Normal of ir_inst list | Returned of ir_inst list

(* 将 stmt_res 展平为代码列表 *)
let flatten = function Normal code | Returned code -> code

(* 检查代码段最后一条是否是 Goto 指定标签或 Return *)
let ends_with_jump_or_return insts =
  match List.rev insts with
  | Goto _ :: _ -> true
  | Ret _ :: _ -> true
  | _ -> false

(* 表达式转换：返回目标寄存器和 IR 指令列表 *)
let rec expr_to_ir (ctx : context) (e : expr) : operand * ir_inst list =
  match e with
  | Number n -> (Imm n, [])
  | ID name ->
      let operand = Env.find name ctx.env in
      (operand, [])
  | Unop (op, e1) -> (
      let operand, code = expr_to_ir ctx e1 in
      match operand with
      | Imm n ->
          let folded =
            match op with
            | Plus -> Imm n (* +n = n *)
            | Minus -> Imm (-n) (* -n *)
            | Not -> Imm (if n = 0 then 1 else 0)
            (* !n *)
          in
          (folded, code)
      | _ ->
          let res = fresh_temp () in
          (res, code @ [ Unop (string_of_unop op, res, operand) ]))
  | Binop (Land, e1, e2) ->
    (* 短路与: a && b *)
    let lhs, c1 = expr_to_ir ctx e1 in
    let res    = fresh_temp () in
    let l_rhs  = fresh_label () in
    let l_end  = fresh_label () in
    let rhs, c2 = expr_to_ir ctx e2 in
    let tmp1   = fresh_temp () in
    let tmp2   = fresh_temp () in
    let code =
      (* 1. 默认 false *)
      [ Assign(res, Imm 0) ]
      (* 2. 计算 lhs *)
      @ c1
      (* 3. t1 = (lhs != 0); if t1 goto rhs  else jump to end *)
      @ [ Binop("!=", tmp1, lhs, Imm 0)
        ; IfGoto (tmp1, l_rhs)
        ; Goto    l_end
        ; Label   l_rhs
        ]
      (* 4. 计算 rhs *)
      @ c2
      (* 5. t2 = (rhs != 0); if t2 goto set_true else jump to end *)
      @ [ Binop("!=", tmp2, rhs, Imm 0)
        ; IfGoto(tmp2, l_end ^ "_set") (* 我们用另一个临时标签 *)
        ; Goto    l_end
        ]
      (* 6. set_true: res = 1 *)
      @ [ Label (l_end ^ "_set")
        ; Assign(res, Imm 1)
        ]
      (* 7. end 标签 *)
      @ [ Label l_end ]
    in
    res, code

| Binop (Lor, e1, e2) ->
    (* 短路或: a || b *)
    let lhs, c1 = expr_to_ir ctx e1 in
    let res    = fresh_temp () in
    let l_rhs  = fresh_label () in
    let l_end  = fresh_label () in
    let rhs, c2 = expr_to_ir ctx e2 in
    let tmp1   = fresh_temp () in
    let tmp2   = fresh_temp () in
    let code =
      (* 1. 默认 false *)
      [ Assign(res, Imm 0) ]
      (* 2. 计算 lhs *)
      @ c1
      (* 3. t1 = (lhs != 0); if t1 goto set_true  else goto rhs *)
      @ [ Binop("!=", tmp1, lhs, Imm 0)
        ; IfGoto(tmp1, l_end ^ "_set")
        ; Goto    l_rhs
        ; Label   l_rhs
        ]
      (* 4. 计算 rhs *)
      @ c2
      (* 5. t2 = (rhs != 0); if t2 goto set_true else goto end *)
      @ [ Binop("!=", tmp2, rhs, Imm 0)
        ; IfGoto(tmp2, l_end ^ "_set")
        ; Goto    l_end
        ]
      (* 6. set_true: res = 1 *)
      @ [ Label (l_end ^ "_set")
        ; Assign(res, Imm 1)
        ]
      (* 7. end 标签 *)
      @ [ Label l_end ]
    in
    res, code
  | Binop (op, e1, e2) -> (
      let lhs, c1 = expr_to_ir ctx e1 in
      let rhs, c2 = expr_to_ir ctx e2 in
      match (lhs, rhs) with
      | Imm a, Imm b ->
          let folded =
            match op with
            | Add -> Imm (a + b)
            | Sub -> Imm (a - b)
            | Mul -> Imm (a * b)
            | Div -> Imm (a / b)
            | Mod -> Imm (a mod b)
            | Eq -> Imm (if a = b then 1 else 0)
            | Neq -> Imm (if a <> b then 1 else 0)
            | Less -> Imm (if a < b then 1 else 0)
            | Leq -> Imm (if a <= b then 1 else 0)
            | Greater -> Imm (if a > b then 1 else 0)
            | Geq -> Imm (if a >= b then 1 else 0)
            | Lor | Land -> failwith "Never touched"
          in
          (folded, c1 @ c2)
      | _ ->
          let dst = fresh_temp () in
          (dst, c1 @ c2 @ [ Binop (string_of_binop op, dst, lhs, rhs) ]))
  | Call (f, args) ->
      (* 参数顺序按出现顺序计算 *)
      let codes, oprs =
        List.fold_left
          (fun (acc_code, acc_opr) arg ->
            let opr, code = expr_to_ir ctx arg in
            (acc_code @ code, acc_opr @ [ opr ]))
          ([], []) args
      in
      let ret = fresh_temp () in
      (ret, codes @ [ Call (ret, f, oprs) ])

(* 语句翻译，返回 Normal/Returned，支持块作用域、break/continue、return 提前终止 *)
let rec stmt_to_res (ctx : context) (s : stmt) : stmt_res =
  match s with
  | Empty -> Normal []
  | ExprStmt e ->
      let _, code = expr_to_ir ctx e in
      Normal code
  | Decl (_x, None) ->
      (* 无初始化的声明仅更新 env *)
      Normal []
  | Decl (x, Some e) ->
      let v, c = expr_to_ir ctx e in
      let var = Var x in
      Normal (c @ [ Assign (var, v) ])
  | Assign (x, e) ->
      let v, c = expr_to_ir ctx e in
      let var = Env.find x ctx.env in
      Normal (c @ [ Assign (var, v) ])
  | Return None -> Returned [ Ret None ]
  | Return (Some e) ->
      let v, c = expr_to_ir ctx e in
      Returned (c @ [ Ret (Some v) ])
  | If (cond, tstmt, Some fstmt) -> (
      let cnd, cc = expr_to_ir ctx cond in
      let lthen = fresh_label ()
      and lelse = fresh_label ()
      and lend = fresh_label () in
      let then_res = stmt_to_res ctx tstmt
      and else_res = stmt_to_res ctx fstmt in
      let raw_then = flatten then_res in
      let then_code =
        if ends_with_jump_or_return raw_then then raw_then
        else raw_then @ [ Goto lend ]
      in
      let raw_else = flatten else_res in
      let else_code =
        if ends_with_jump_or_return raw_else then raw_else
        else raw_else @ [ Goto lend ]
      in
      let code =
        cc
        @ [ IfGoto (cnd, lthen); Goto lelse ]
        @ [ Label lthen ] @ then_code @ [ Label lelse ] @ else_code
        @ [ Label lend ]
      in
      match (then_res, else_res) with
      | Returned _, _ | _, Returned _ -> Returned code
      | _ -> Normal code)
  | If (cond, tstmt, None) ->
      let cnd, cc = expr_to_ir ctx cond in
      let lthen = fresh_label () and lskip = fresh_label () in
      let then_res = stmt_to_res ctx tstmt in
      let then_code = flatten then_res in
      let code =
        cc
        @ [ IfGoto (cnd, lthen); Goto lskip ]
        @ [ Label lthen ] @ then_code @ [ Label lskip ]
      in
      Normal code
  | While (cond, body) ->
      (* 循环标签 *)
      let lcond = fresh_label ()
      and lbody = fresh_label ()
      and lend = fresh_label () in
      let ctx_loop =
        { ctx with break_lbl = Some lend; continue_lbl = Some lcond }
      in
      let cnd, ccode = expr_to_ir ctx_loop cond in
      let body_res = stmt_to_res ctx_loop body in
      let bcode = flatten body_res in
      let code =
        [ Goto lcond; Label lcond ]
        @ ccode
        @ [ IfGoto (cnd, lbody); Goto lend ]
        @ [ Label lbody ] @ bcode @ [ Goto lcond; Label lend ]
      in
      (* 无法从循环体中直接 return：若想支持可在 body_res 捕获 *)
      Normal code
  | Break -> (
      match ctx.break_lbl with
      | Some lbl -> Normal [ Goto lbl ]
      | None -> failwith "break used outside loop")
  | Continue -> (
      match ctx.continue_lbl with
      | Some lbl -> Normal [ Goto lbl ]
      | None -> failwith "continue used outside loop")
  | Block stmts ->
      (* 块作用域隔离 *)
      let saved_env = ctx.env in
      let rec loop env_acc acc_code = function
        | [] -> Normal acc_code
        | hd :: tl -> (
            let ctx' = { ctx with env = env_acc } in
            let res = stmt_to_res ctx' hd in
            let code_hd = flatten res in
            let env_next =
              match hd with
              | Decl (x, _) -> Env.add x (Var x) env_acc
              | _ -> env_acc
            in
            match res with
            | Normal _ -> loop env_next (acc_code @ code_hd) tl
            | Returned _ -> Returned (acc_code @ code_hd))
      in
      (* 执行块体 *)
      let res = loop saved_env [] stmts in
      (* 恢复外层 env *)
      ctx.env <- saved_env;
      res

(* 函数转换 *)
let func_to_ir (f : func_def) : ir_func =
  (* 初始化 env: 参数映射 *)
  let init_env =
    List.fold_left (fun m x -> Env.add x (Var x) m) Env.empty f.params
  in
  let ctx0 = { env = init_env; break_lbl = None; continue_lbl = None } in
  (* 翻译函数体 *)
  let body_res = stmt_to_res ctx0 (Block f.body) in
  (* 先拿到全部 IR 指令 *)
  let raw_code = flatten body_res in
  (* 如果末尾恰好是一个孤立 Label，就把它丢掉 *)
  let body_code =
    match List.rev raw_code with
    | Label _ :: rest_rev -> List.rev rest_rev
    | _ -> raw_code
  in
  { name = f.func_name; args = f.params; body = body_code }

(* 线性IR -> 过程块IR *)
let partition_blocks (insts : ir_inst list) : ir_block list =
  let rec split acc curr label label_map insts =
    match insts with
    | [] -> (
        match curr with
        | [] -> List.rev acc
        | _ -> failwith "Basic block must end with a terminator")
    | Label l :: rest -> (
        (* 当前块结束，开启新块 *)
        match curr with
        | [] ->
            let next_label, label_map' = fresh_IRlabel label_map l in
            split acc [ Label l ] next_label label_map' rest
        | _ ->
            let next_label, label_map' = fresh_IRlabel label_map l in
            let blk =
              {
                label;
                insts = List.rev curr;
                terminator = TermSeq next_label;
                preds = [];
                succs = [];
              }
            in
            let acc' = blk :: acc in
            split acc' [ Label l ] next_label label_map' rest)
    | Goto l :: rest ->
        let goto_label, label_map' = fresh_IRlabel label_map l in
        (* 刷新一个无意义的 blk, 确保编程者不会出现的 label *)
        let next_label, label_map'' =
          fresh_IRlabel label_map' ("__blk" ^ string_of_int !ir_label_id)
        in
        let blk =
          {
            label;
            insts = List.rev (Goto l :: curr);
            terminator = TermGoto goto_label;
            preds = [];
            succs = [];
          }
        in
        split (blk :: acc) [] next_label label_map'' rest
    | IfGoto (cond, l) :: rest ->
        let then_label, label_map' = fresh_IRlabel label_map l in
        let else_label, label_map'' =
          fresh_IRlabel label_map' ("__else" ^ string_of_int !ir_label_id)
        in
        let blk =
          {
            label;
            insts = List.rev (IfGoto (cond, l) :: curr);
            terminator = TermIf (cond, then_label, else_label);
            preds = [];
            succs = [];
          }
        in
        split (blk :: acc) [] else_label label_map'' rest
    | Ret op :: rest ->
        let next_label, label_map' =
          fresh_IRlabel label_map ("__ret" ^ string_of_int !ir_label_id)
        in
        let blk =
          {
            label;
            insts = List.rev (Ret op :: curr);
            terminator = TermRet op;
            preds = [];
            succs = [];
          }
        in
        split (blk :: acc) [] next_label label_map' rest
    | inst :: rest -> split acc (inst :: curr) label label_map rest
  in
  (* 确保用户不使用 entry 标签 *)
  let entry_label, label_map = fresh_IRlabel LabelMap.empty "entry" in
  split [] [] entry_label label_map insts

(* 优化版本的 ir 控制块 *)
let func_to_ir_o (f : func_def) : ir_func_o =
  temp_id := 0;
  label_id := 0;
  let init_env =
    List.fold_left
      (fun acc name -> Env.add name (Var name) acc)
      Env.empty f.params
  in
  let ctx0 = { env = init_env; break_lbl = None; continue_lbl = None } in
  let body_code = stmt_to_res ctx0 (Block f.body) |> flatten in
  let linear_ir =
    (* 额外处理孤立的 label *)
    match List.rev body_code with
    | Label _ :: rest_rev -> List.rev rest_rev
    | _ -> body_code
  in
  let raw_blocks = partition_blocks linear_ir in
  (* 构建前驱/后继关系，并剔除空块/重复块 *)
  let cfg_blocks = Cfg.build_cfg raw_blocks in
  let opt_blocks = Cfg.optimize cfg_blocks in
  { name = f.func_name; args = f.params; blocks = opt_blocks }

(* 编译单元转换 *)
let program_to_ir (cu : comp_unit) (optimize_flag : bool) : ir_program =
  if optimize_flag then Ir_funcs_o (List.map func_to_ir_o cu)
  else Ir_funcs (List.map func_to_ir cu)
