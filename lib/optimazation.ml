open Ir

(* 对某条指令求解 def、use *)
let def_use_inst (inst : ir_inst) : StringSet.t * StringSet.t =
  let op_to_varset = function
    (* 返回 Set[x] *)
    | Var x | Reg x -> StringSet.singleton x
    | Imm _ -> StringSet.empty
  in
  match inst with
  | Binop (_, dst, lhs, rhs) ->
      let def = op_to_varset dst in
      let use = StringSet.union (op_to_varset lhs) (op_to_varset rhs) in
      (def, use)
  | Unop (_, dst, src) ->
      let def = op_to_varset dst in
      let use = op_to_varset src in
      (def, use)
  | Assign (dst, src) -> (op_to_varset dst, op_to_varset src)
  | Load (dst, src) -> (op_to_varset dst, op_to_varset src)
  | Store (dst, src) ->
      (StringSet.empty, StringSet.union (op_to_varset dst) (op_to_varset src))
  | Call (dst, _, args) ->
      ( op_to_varset dst,
        List.fold_left
          (fun acc a -> StringSet.union acc (op_to_varset a))
          StringSet.empty args )
  | _ -> (StringSet.empty, StringSet.empty)

let def_use_block (blk : ir_block) : StringSet.t * StringSet.t =
  List.fold_right
    (* 对每一条指令，根据后面指令的 def、use 来得到当前指令的 def、use *)
    (* 因为是相对于 B 的后继块 OUT(B) 去做差集, 差集后不能又被 use(B) 并回去 *)
    (* 即 IN(B) = (OUT(B) + (use(B) - def(B)) ), 后一项看成 use' *)
    (* 所以是 fold_right *)
    (fun inst (defs, uses) ->
      let def, use = def_use_inst inst in
      (* 一定是后面没有定义过的 use 才算数 *)
      let new_use = StringSet.union (StringSet.diff use defs) uses in
      (* 所有的 def 都算数 *)
      let new_def = StringSet.union def defs in
      (new_def, new_use))
    blk.insts
    (StringSet.empty, StringSet.empty)

(* 设置 blocks 的 live_in 和 live_out 信息 *)
let liveness_analysis (blocks : ir_block list) : unit =
  let changed = ref true in
  (* 初始化 live_in 和 live_out 信息 *)
  List.iter
    (fun blk ->
      blk.live_in <- StringSet.empty;
      blk.live_out <- StringSet.empty)
    blocks;

  while !changed do
    changed := false;
    List.iter
      (fun blk ->
        (* OUT(B) = U IN(S), for all S in succ(B) *)
        let succ_live_in =
          List.fold_left
            (* 对给定的 lbl 后继, 把它的 In(S) 给拿到 *)
            (fun acc lbl ->
              (* 在 CFG 里面确保存在这样的 lbl 块 *)
              match List.find_opt (fun b -> b.label = lbl) blocks with
              | Some b -> StringSet.union acc b.live_in
              | None -> acc)
            StringSet.empty blk.succs
        in

        (* 以过程块为单位求解 def 和 use *)
        let def, use = def_use_block blk in
        (* IN(B) = (OUT(B) - def(B)) U use(B) *)
        let new_in = StringSet.union use (StringSet.diff succ_live_in def) in

        (* 检查是否不再变化 *)
        if
          (not (StringSet.equal blk.live_in new_in))
          || not (StringSet.equal blk.live_out succ_live_in)
        then changed := true;

        blk.live_in <- new_in;
        blk.live_out <- succ_live_in)
      blocks
  done
