(* cfg.ml *)
open Ir
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type const_state = int option
(* 映射变量名 -> 常量值（或None表示非常量） *)
type const_env = const_state StringMap.t

(* 合并多个环境的状态 *)
let merge_envs (envs : const_env list) : const_env =
  if envs = [] then StringMap.empty
  else
    let all_vars = List.fold_left (fun acc env ->
        StringMap.fold (fun k _ acc -> StringSet.add k acc) env acc
      ) StringSet.empty envs in
    StringSet.fold (fun var acc ->
        let states = List.map (fun env ->
            try StringMap.find var env with Not_found -> None
          ) envs in
        let is_same = match states with
          | [] -> true
          | hd::tl -> List.for_all ((=) hd) tl
        in
        if is_same then
          StringMap.add var (List.hd states) acc
        else
          StringMap.add var None acc
      ) all_vars StringMap.empty

let eval_operand env op =
  match op with
  | Var name | Reg name ->
      (try match StringMap.find name env with
       | Some i -> Imm i
       | None -> op
       with Not_found -> op)
  | Imm _ -> op

let eval_binop op op1 op2 =
  match (op1, op2) with
  | (Imm a, Imm b) ->
    (match op with
     | "+" -> Some (a + b)
     | "-" -> Some (a - b)
     | "*" -> Some (a * b)
     | "/" when b <> 0 -> Some (a / b)
     | "%" when b <> 0 -> Some (a mod b)
     | "==" -> Some (if a = b then 1 else 0)
     | "!=" -> Some (if a <> b then 1 else 0)
     | "<" -> Some (if a < b then 1 else 0)
     | "<=" -> Some (if a <= b then 1 else 0)
     | ">" -> Some (if a > b then 1 else 0)
     | ">=" -> Some (if a >= b then 1 else 0)
     | _ -> None)
  | _ -> None

let eval_unop op op1 =
  match op1 with
  | Imm a ->
    (match op with
     | "!" -> Some (if a = 0 then 1 else 0)
     | "-" -> Some (-a)
     | "+" -> Some a
     | _ -> None)
  | _ -> None

let process_inst env inst =
  match inst with
  | Assign (dst, src) ->
      let src' = eval_operand env src in
      let env' =
        match dst with
        | Var name | Reg name ->
            (match src' with
             | Imm i -> StringMap.add name (Some i) env
             | _ -> StringMap.add name None env)
        | _ -> env
      in
      Assign (dst, src'), env'

  | Binop (op, dst, src1, src2) ->
      let src1' = eval_operand env src1 in
      let src2' = eval_operand env src2 in
      (match eval_binop op src1' src2' with
       | Some i -> Assign (dst, Imm i), StringMap.add (match dst with Var v | Reg v -> v | _ -> "") (Some i) env
       | None -> Binop (op, dst, src1', src2'), StringMap.add (match dst with Var v | Reg v -> v | _ -> "") None env)

  | Unop (op, dst, src) ->
      let src' = eval_operand env src in
      (match eval_unop op src' with
       | Some i -> Assign (dst, Imm i), StringMap.add (match dst with Var v | Reg v -> v | _ -> "") (Some i) env
       | None -> Unop (op, dst, src'), StringMap.add (match dst with Var v | Reg v -> v | _ -> "") None env)

  | Call (dst, fname, args) ->
      let args' = List.map (eval_operand env) args in
      let env' = match dst with Var v | Reg v -> StringMap.add v None env | _ -> env in
      Call (dst, fname, args'), env'

  | Load (dst, addr) ->
      let addr' = eval_operand env addr in
      let env' = match dst with Var v | Reg v -> StringMap.add v None env | _ -> env in
      Load (dst, addr'), env'

  | Store (addr, value) ->
      let addr' = eval_operand env addr in
      let value' = eval_operand env value in
      Store (addr', value'), env

  | IfGoto (cond, label) ->
      IfGoto (eval_operand env cond, label), env

  | Ret op_opt ->
      Ret (Option.map (eval_operand env) op_opt), env

  | Goto _ | Label _ as t -> t, env

let process_terminator env term =
  match term with
  | TermIf (cond, l1, l2) -> TermIf (eval_operand env cond, l1, l2)
  | TermRet o -> TermRet (Option.map (eval_operand env) o)
  | TermGoto _ | TermSeq _ as t -> t

let build_cfg (blocks : ir_block list) : ir_block list =
  let label_map = Hashtbl.create 10 in
  List.iter (fun blk -> Hashtbl.add label_map blk.label blk) blocks;
  let succ_map = Hashtbl.create 10 in
  let pred_map = Hashtbl.create 10 in
  List.iter (fun blk ->
    Hashtbl.replace succ_map blk.label [];
    Hashtbl.replace pred_map blk.label []
  ) blocks;
  List.iter (fun blk ->
    let succs = match blk.terminator with
      | TermGoto l -> [l]
      | TermIf (_, l1, l2) -> [l1; l2]
      | TermSeq l -> [l]
      | TermRet _ -> []
    in
    Hashtbl.replace succ_map blk.label succs;
    List.iter (fun s ->
      let old = try Hashtbl.find pred_map s with _ -> [] in
      Hashtbl.replace pred_map s (blk.label :: old)) succs
  ) blocks;
  List.iter (fun blk ->
    blk.succs <- (try Hashtbl.find succ_map blk.label with _ -> []);
    blk.preds <- (try Hashtbl.find pred_map blk.label with _ -> [])
  ) blocks;
  blocks

let constant_propagation (blocks : ir_block list) : ir_block list =
  let block_map = List.fold_left (fun m b -> StringMap.add b.label b m) StringMap.empty blocks in
  let in_envs = ref StringMap.empty in
  let out_envs = ref StringMap.empty in
  List.iter (fun b ->
    in_envs := StringMap.add b.label StringMap.empty !in_envs;
    out_envs := StringMap.add b.label StringMap.empty !out_envs
  ) blocks;
  let worklist = Queue.create () in
  List.iter (fun b -> Queue.add b.label worklist) blocks;
  while not (Queue.is_empty worklist) do
    let label = Queue.take worklist in
    let blk = StringMap.find label block_map in
    let preds = blk.preds in
    let in_env =
      if preds = [] then StringMap.empty
      else merge_envs (List.map (fun p -> StringMap.find p !out_envs) preds)
    in
    in_envs := StringMap.add label in_env !in_envs;
    let new_insts, out_env = List.fold_left (fun (acc, env) inst ->
      let inst', env' = process_inst env inst in
      acc @ [inst'], env'
    ) ([], in_env) blk.insts in
    let new_term = process_terminator out_env blk.terminator in
    let old_out = StringMap.find label !out_envs in
    if not (StringMap.equal (=) out_env old_out) then begin
      out_envs := StringMap.add label out_env !out_envs;
      List.iter (fun succ -> Queue.add succ worklist) blk.succs
    end;
    blk.insts <- new_insts;
    blk.terminator <- new_term;
  done;
  blocks

let optimize blocks =
  blocks |> build_cfg |> constant_propagation
