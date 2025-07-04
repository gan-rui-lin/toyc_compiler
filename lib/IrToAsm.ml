open Ir

let stack_offset = ref 0
let var_env = Hashtbl.create 1024

(* 寄存器分配相关 *)
let reg_map : (string, string) Hashtbl.t = Hashtbl.create 64
let spilled_vars : (string, int) Hashtbl.t = Hashtbl.create 64
let reg_pool = ref [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6" ]

(* 寄存器号, spill 符 *)
let allocate_reg var live_out : string * string option =
  (* 查找是否已分配寄存器 *)
  if Hashtbl.mem reg_map var then (Hashtbl.find reg_map var, None)
  else
    match !reg_pool with
    | r :: rest ->
        reg_pool := rest;
        Hashtbl.add reg_map var r;
        (r, None)
    | [] -> (
        (* 寻找要覆盖 寄存器 *)
        let victim_opt =
          Hashtbl.fold
            (* 对 variable-register 键值对做分析 *)
            (fun v r acc ->
              (* 活着就不动 *)
              if StringSet.mem v live_out then acc else Some (v, r))
            reg_map None
        in
        match victim_opt with
        | Some (victim, reg) ->
            if StringSet.mem victim live_out then
              (* 仍然活跃，spill *)
              let offset =
                if Hashtbl.mem spilled_vars victim then
                  Hashtbl.find spilled_vars victim
                else (
                  stack_offset := !stack_offset - 4;
                  Hashtbl.add spilled_vars victim !stack_offset;
                  !stack_offset)
              in
              ( reg,
                Some
                  (Printf.sprintf "\tsw %s, %d(sp) # spill %s\n" reg offset
                     victim) )
            else
              ( (* 死变量，根本不用 spill *)
                reg,
                None )
        (* 找不到要覆盖的寄存器 *)
        | None -> failwith "All variables live, cannot spill")

let fresh_temp () : string =
  match !reg_pool with
  | r :: rest ->
      reg_pool := rest;
      r
  | [] -> failwith "No free temporary registers available"

let free_temp reg = reg_pool := reg :: !reg_pool

let get_stack_offset var =
  try Hashtbl.find var_env var
  with Not_found -> failwith ("Unknown variable: " ^ var)

(* 变量是否已经在符号表里面了, 存在则直接返回偏移, 否则分配新偏移 *)
let alloc_stack var =
  try get_stack_offset var
  with _ ->
    stack_offset := !stack_offset + 4;
    Hashtbl.add var_env var !stack_offset;
    !stack_offset

let operand_to_str = function
  | Reg r | Var r -> Printf.sprintf "%d(sp)" (get_stack_offset r)
  | Imm i -> Printf.sprintf "%d" i

let load_operand (reg : string) (op : operand) : string =
  match op with
  | Imm i -> Printf.sprintf "\tli %s, %d\n" reg i
  | Reg r | Var r -> Printf.sprintf "\tlw %s, %d(sp)\n" reg (get_stack_offset r)

let compile_inst (inst : ir_inst) : string =
  match inst with
  | Binop (op, dst, lhs, rhs) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let lhs_code = load_operand "t1" lhs in
      let rhs_code = load_operand "t2" rhs in
      let op_code =
        match op with
        | "+" -> "\tadd t0, t1, t2\n"
        | "-" -> "\tsub t0, t1, t2\n"
        | "*" -> "\tmul t0, t1, t2\n"
        | "/" -> "\tdiv t0, t1, t2\n"
        | "%" -> "\trem t0, t1, t2\n"
        | "==" -> "\tsub t0, t1, t2\n\tseqz t0, t0\n"
        | "!=" -> "\tsub t0, t1, t2\n\tsnez t0, t0\n"
        | "<=" -> "\tsgt t0, t1, t2\n\txori t0, t0, 1\n"
        | ">=" -> "\tslt t0, t1, t2\n\txori t0, t0, 1\n"
        | "<" -> "\tslt t0, t1, t2\n"
        | ">" -> "\tsgt t0, t1, t2\n"
        | "&&" -> "\tand t0, t1, t2\n"
        | "||" -> "\tor t0, t1, t2\n"
        | _ -> failwith ("Unknown binop: " ^ op)
      in
      lhs_code ^ rhs_code ^ op_code ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  | Unop (op, dst, src) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let load_src = load_operand "t1" src in
      let op_code =
        match op with
        | "-" -> "\tneg t0, t1\n"
        | "!" -> "\tseqz t0, t1\n"
        | "+" -> "\tmv t0, t1\n"
        | _ -> failwith ("Unknown unop: " ^ op)
      in
      load_src ^ op_code ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  | Assign (dst, src) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let load_src = load_operand "t0" src in
      load_src ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  (* Not used *)
  | Load (dst, src) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let src_code = load_operand "t1" src in
      src_code ^ "\tlw t0, 0(t1)\n" ^ Printf.sprintf "\tsw t0, %d(sp)\n" dst_off
  (* Not used *)
  | Store (dst, src) ->
      let dst_code = load_operand "t1" dst in
      let src_code = load_operand "t2" src in
      dst_code ^ src_code ^ "\tsw t2, 0(t1)\n"
  | Call (dst, fname, args) ->
      let dst_off =
        alloc_stack
          (match dst with Reg r | Var r -> r | _ -> failwith "Bad dst")
      in
      let args_code =
        List.mapi
          (fun i arg ->
            if i < 8 then load_operand (Printf.sprintf "a%d" i) arg
            else
              let offset = 4 * (i - 8) in
              load_operand "t0" arg ^ Printf.sprintf "\tsw t0, %d(sp)\n" (-1600 - offset))
          args
        |> String.concat ""
      in
      args_code ^ Printf.sprintf "\tcall %s\n\tsw a0, %d(sp)\n" fname dst_off
  | Ret None ->
      let ra_offset = get_stack_offset "ra" in
      Printf.sprintf
        "\tlw ra, %d(sp)\n\taddi sp, sp, 1600\n\tret\n"
        ra_offset
  | Ret (Some op) ->
      let load_code = load_operand "a0" op in
      let ra_offset = get_stack_offset "ra" in
      load_code
      ^ Printf.sprintf
          "\tlw ra, %d(sp)\n\taddi sp, sp, 1600\n\tret\n"
          ra_offset
  | Goto label -> Printf.sprintf "\tj %s\n" label
  | IfGoto (cond, label) ->
      let cond_code = load_operand "t0" cond in
      cond_code ^ Printf.sprintf "\tbne t0, x0, %s\n" label
  | Label label -> Printf.sprintf "%s:\n" label

let compile_inst_with_liveness (inst : ir_inst) (live_out : StringSet.t) :
    string =
  let get_var = function Reg x | Var x -> x | Imm _ -> "IMM" in
  (* 指令, 操作数, spill 符 *)
  let load_operand_to_reg reg = function
    | Imm i -> (Printf.sprintf "\tli %s, %d\n" reg i, reg, None)
    | Var v | Reg v ->
        (* 尝试分配寄存器 *)
        let r, spill = allocate_reg v live_out in
        if r = reg then ("", reg, spill)
        else (Printf.sprintf "\tmv %s, %s\n" reg r, reg, spill)
    (* 如果和预分配寄存器不一致，那么执行 mv 指令 *)
  in
  let load_operand_free = function
    | Imm i ->
        let reg = fresh_temp () in
        (Printf.sprintf "\tli %s, %d\n" reg i, reg, None)
    | Var v | Reg v ->
        let reg, spill = allocate_reg v live_out in
        ("", reg, spill)
  in
  match inst with
  | Binop (op, dst, lhs, rhs) ->
      let dst_v = get_var dst in
      let dst_reg, dst_spill = allocate_reg dst_v live_out in
      let lhs_code, lhs_reg, lhs_spill = load_operand_free lhs in
      let rhs_code, rhs_reg, rhs_spill = load_operand_free rhs in
      let op_code =
        match op with
        | "+" -> Printf.sprintf "\tadd %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "-" -> Printf.sprintf "\tsub %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "*" -> Printf.sprintf "\tmul %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "/" -> Printf.sprintf "\tdiv %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "%" -> Printf.sprintf "\trem %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "==" ->
            Printf.sprintf "\tsub %s, %s, %s\n\tseqz %s, %s\n" dst_reg lhs_reg
              rhs_reg dst_reg dst_reg
        | "!=" ->
            Printf.sprintf "\tsub %s, %s, %s\n\tsnez %s, %s\n" dst_reg lhs_reg
              rhs_reg dst_reg dst_reg
        | "<" -> Printf.sprintf "\tslt %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | ">" -> Printf.sprintf "\tsgt %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "<=" ->
            Printf.sprintf "\tsgt %s, %s, %s\n\txori %s, %s, 1\n" dst_reg
              lhs_reg rhs_reg dst_reg dst_reg
        | ">=" ->
            Printf.sprintf "\tslt %s, %s, %s\n\txori %s, %s, 1\n" dst_reg
              lhs_reg rhs_reg dst_reg dst_reg
        | "&&" -> Printf.sprintf "\tand %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | "||" -> Printf.sprintf "\tor %s, %s, %s\n" dst_reg lhs_reg rhs_reg
        | _ -> failwith ("Unknown binop: " ^ op)
      in
      let spill_code =
        [ lhs_spill; rhs_spill; dst_spill ]
        |> List.filter_map Fun.id |> String.concat ""
      in
      (* 移除死变量对应的临时寄存器 *)
      (match lhs with
      | Imm _ -> free_temp lhs_reg
      | Var v | Reg v ->
          if not (StringSet.mem v live_out) then free_temp lhs_reg);
      (match rhs with
      | Imm _ -> free_temp rhs_reg
      | Var v | Reg v ->
          if not (StringSet.mem v live_out) then free_temp rhs_reg);
      (* 先spill 变量 *)
      spill_code ^ lhs_code ^ rhs_code ^ op_code
  | Unop (op, dst, src) ->
      let src_code, src_reg, src_spill = load_operand_free src in
      let dst_v = get_var dst in
      let dst_reg, dst_spill = allocate_reg dst_v live_out in
      let op_code =
        match op with
        | "-" -> Printf.sprintf "\tneg %s, %s\n" dst_reg src_reg
        | "!" -> Printf.sprintf "\tseqz %s, %s\n" dst_reg src_reg
        | "+" -> Printf.sprintf "\tmv %s, %s\n" dst_reg src_reg
        | _ -> failwith ("Unknown unop: " ^ op)
      in

      (match src with
      | Imm _ -> free_temp src_reg
      | Var v | Reg v ->
          if not (StringSet.mem v live_out) then free_temp src_reg);
      ([ src_spill; dst_spill ] |> List.filter_map Fun.id |> String.concat "")
      ^ src_code ^ op_code
  | Assign (dst, src) ->
      let dst_v = get_var dst in
      let dst_reg, dst_spill = allocate_reg dst_v live_out in
      let src_code, src_reg, src_spill = load_operand_free src in
      (match src with
      | Imm _ -> free_temp src_reg
      | Var v | Reg v ->
          if not (StringSet.mem v live_out) then free_temp src_reg);
      ([ src_spill; dst_spill ] |> List.filter_map Fun.id |> String.concat "")
      ^ src_code
      ^ Printf.sprintf "\tmv %s, %s\n" dst_reg src_reg
  | Store (addr, value) ->
      let addr_code, addr_reg, addr_spill = load_operand_to_reg "t1" addr in
      let val_code, val_reg, val_spill = load_operand_to_reg "t2" value in
      ([ addr_spill; val_spill ] |> List.filter_map Fun.id |> String.concat "")
      ^ addr_code ^ val_code
      ^ Printf.sprintf "\tsw %s, 0(%s)\n" val_reg addr_reg
  | Load (dst, addr) ->
      let addr_code, addr_reg, addr_spill = load_operand_to_reg "t1" addr in
      let dst_v = get_var dst in
      let dst_reg, dst_spill = allocate_reg dst_v live_out in
      ([ addr_spill; dst_spill ] |> List.filter_map Fun.id |> String.concat "")
      ^ addr_code
      ^ Printf.sprintf "\tlw %s, 0(%s)\n" dst_reg addr_reg
  | Call (dst, fname, args) ->
      let arg_code =
        args
        |> List.mapi (fun i arg ->
               (* TODO : MORE THEN 8 PARAMS *)
               let reg =
                 if i < 8 then Printf.sprintf "a%d" i else "TODO_STACK"
               in
               let op_code, op_reg, op_spill = load_operand_to_reg reg arg in
               let move_code =
                 if reg = op_reg then ""
                 else Printf.sprintf "\tmv %s, %s\n" reg op_reg
               in
               (op_spill |> Option.value ~default:"") ^ op_code ^ move_code)
        |> String.concat ""
      in
      let dst_v = get_var dst in
      (* Printf.printf "dst_v = %s" dst_v; *)
      let dst_reg, dst_spill = allocate_reg dst_v live_out in

      (* Printf.printf "dst_reg = %s" dst_reg; *)
      arg_code ^ "\tcall " ^ fname ^ "\n"
      ^ Option.value ~default:"" dst_spill
      ^ if dst_reg <> "a0" then Printf.sprintf "\tmv %s, a0\n" dst_reg else ""
  | IfGoto (cond, label) ->
      let cond_code, cond_reg, cond_spill = load_operand_to_reg "t0" cond in
      Option.value ~default:"" cond_spill
      ^ cond_code
      ^ Printf.sprintf "\tbne %s, x0, %s\n" cond_reg label
  | Goto label -> Printf.sprintf "\tj %s\n" label
  | Label label -> Printf.sprintf "%s:\n" label
  | Ret None ->
      let ra_offset = get_stack_offset "ra" in
      Printf.sprintf "\tlw ra, %d(sp)\n\taddi sp, sp, 1600\n\tret\n" ra_offset
  | Ret (Some op) ->
      (* 如果变量不在 a0 寄存器里面, mv 一下 *)
      let code, _, spill = load_operand_to_reg "a0" op in
      let ra_offset = get_stack_offset "ra" in
      Option.value ~default:"" spill
      ^ code
      ^ Printf.sprintf "\tlw ra, %d(sp)\n\taddi sp, sp, 1600\n\tret\n" ra_offset

let compile_block (blk : ir_block) : string =
  let code_acc = ref [] in
  let live = ref blk.live_out in

  (* 倒序遍历指令，并处理活跃变量 *)
  List.iter
    (fun inst ->
      let def, use = Optimazation.def_use_inst inst in
      let live_out = !live in
      let inst_code = compile_inst_with_liveness inst live_out in
      code_acc := inst_code :: !code_acc;
      (* 更新当前指令结束后的 live *)
      live := StringSet.union (StringSet.diff !live def) use)
    (List.rev blk.insts);

  String.concat "" !code_acc

let compile_func (f : ir_func) : string =
  Hashtbl.clear var_env;
  stack_offset := 0;

  (* 参数入栈 *)
  let param_setup =
    List.mapi
      (fun i name ->
        let off = alloc_stack name in
        if i < 8 then Printf.sprintf "\tsw a%d, %d(sp)\n" i off
        else
          Printf.sprintf "\tlw t0, %d(sp)\n\tsw t0, %d(sp)\n"
            (* offset 为 call 语句将第 i 个参数压入的偏移 *)
            (-4 * (i - 8))
            off)
      f.args
    |> String.concat ""
  in

  (* ra 入栈 *)
  let param_setup =
    param_setup ^ Printf.sprintf "\tsw ra, %d(sp)\n" (alloc_stack "ra")
  in

  let body_code = f.body |> List.map compile_inst |> String.concat "" in

  let body_code =
    if not (String.ends_with ~suffix:"\tret\n" body_code) then
      body_code
      ^ Printf.sprintf
          "\tlw ra, %d(sp)\n\taddi sp, sp, 1600\n\tret\n"
          (get_stack_offset "ra")
    else body_code
  in
  let func_label = f.name in
  let prologue = Printf.sprintf "%s:\n\taddi sp, sp, -1600\n" func_label in
  prologue ^ param_setup ^ body_code

let compile_func_o (f : ir_func_o) : string =
  Hashtbl.clear var_env;
  Hashtbl.clear reg_map;
  Hashtbl.clear spilled_vars;
  stack_offset := 0;

  Optimazation.liveness_analysis f.blocks;

  (* 映射参数名到 a0-a7 *)
  List.iteri
    (fun i name ->
      let reg = Printf.sprintf "a%d" i in
      Hashtbl.add reg_map name reg)
    f.args;

  let param_setup =
    List.mapi
      (fun i name ->
        let off = alloc_stack name in
        Printf.sprintf "\tsw a%d, %d(sp)\n" i off)
      f.args
    |> String.concat ""
  in

  (* ra 入栈 *)
  let param_setup =
    param_setup ^ Printf.sprintf "\tsw ra, %d(sp)\n" (alloc_stack "ra")
  in

  let body_code =
    f.blocks |> List.map (fun blk -> compile_block blk) |> String.concat ""
  in


  (* 检查 body_code 是否以 ret 结束; 没有默认添加 "\taddi sp, sp, 1600\n\tret\n" 语句; 其实可以前移到 IR 阶段 *)
  let body_code =
    if not (String.ends_with ~suffix:"\tret\n" body_code) then
      body_code
      ^ Printf.sprintf
          "\tlw ra, %d(sp)\n\taddi sp, sp, 1600\n\tret\n"
          (get_stack_offset "ra")
    else body_code
  in

  let func_label = f.name in
  let prologue = Printf.sprintf "%s:\n\taddi sp, sp, -1600\n" func_label in
  prologue ^ param_setup ^ body_code

let compile_program (prog : ir_program) : string =
  let prologue = ".text\n .global main\n" in
  let body_asm =
    match prog with
    | Ir_funcs funcs -> List.map compile_func funcs |> String.concat "\n"
    | Ir_funcs_o funcs_o ->
        List.map compile_func_o funcs_o |> String.concat "\n"
  in
  prologue ^ body_asm
