open Ir

let stack_offset = ref 0
let var_env = Hashtbl.create 1600

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
        "\tlw ra, %d(sp)\n\taddi sp, sp, 800\n\taddi sp,sp,800\n\tret\n"
        ra_offset
  | Ret (Some op) ->
      let load_code = load_operand "a0" op in
      let ra_offset = get_stack_offset "ra" in
      load_code
      ^ Printf.sprintf
          "\tlw ra, %d(sp)\n\taddi sp, sp, 800\n\taddi sp,sp,800\n\tret\n"
          ra_offset
  | Goto label -> Printf.sprintf "\tj %s\n" label
  | IfGoto (cond, label) ->
      let cond_code = load_operand "t0" cond in
      cond_code ^ Printf.sprintf "\tbne t0, x0, %s\n" label
  | Label label -> Printf.sprintf "%s:\n" label

let compile_block (blk : ir_block) : string =
  blk.insts |> List.map compile_inst |> String.concat ""

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
          "\tlw ra, %d(sp)\n\taddi sp, sp, 800\n\taddi sp,sp,800\n\tret\n"
          (get_stack_offset "ra")
    else body_code
  in
  let func_label = f.name in
  let prologue = Printf.sprintf "%s:\n\taddi sp, sp, -1600\n" func_label in
  prologue ^ param_setup ^ body_code

let compile_func_o (f : ir_func_o) : string =
  Hashtbl.clear var_env;
  stack_offset := 0;

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

  let body_code = f.blocks |> List.map compile_block |> String.concat "" in

  (* 检查 body_code 是否以 ret 结束; 没有默认添加 "\taddi sp, sp, 800\n\taddi sp,sp,800\n\tret\n" 语句; 其实可以前移到 IR 阶段 *)
  let body_code =
    if not (String.ends_with ~suffix:"\tret\n" body_code) then
      body_code
      ^ Printf.sprintf
          "\tlw ra, %d(sp)\n\taddi sp, sp, 800\n\taddi sp,sp,800\n\tret\n"
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
