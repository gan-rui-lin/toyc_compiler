
open Ir
module StringMap = Map.Make (String)

(* 构建 CFG：为每个 block 设置 succs 和 preds *)
let build_cfg (blocks : ir_block list) : unit =
  (* 构造 label -> block 的映射 *)
  let label_map =
    List.fold_left
      (fun acc blk -> StringMap.add blk.label blk acc)
      StringMap.empty blocks
  in

  (* 对每个 block，根据 terminator 添加后继和前驱关系 *)
  List.iter
    (fun blk ->
      let add_succ l =
        match StringMap.find_opt l label_map with
        | Some succ_blk ->
            blk.succs <- l :: blk.succs;
            succ_blk.preds <- blk.label :: succ_blk.preds
        | None -> failwith ("build_cfg: Label " ^ l ^ " not found")
      in
      match blk.terminator with
      | TermGoto l -> add_succ l
      | TermIf (_, l1, l2) ->
          add_succ l1;
          add_succ l2
      | TermRet _ -> () (* 无后继 *)
      | TermUnreachable -> ())
    blocks
