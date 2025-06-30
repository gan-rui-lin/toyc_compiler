open Ir
module StringMap = Map.Make(String)

(* 快速按标签查找 block *)
let build_label_map blocks : ir_block StringMap.t =
  List.fold_left (fun m blk -> StringMap.add blk.label blk m) StringMap.empty blocks

let find_block_by_label label blocks =
  let label_map = build_label_map blocks in
  StringMap.find_opt label label_map

(* 清空所有 preds/succs *)
let reset_preds_succs blocks =
  List.iter (fun blk -> blk.preds <- []; blk.succs <- []) blocks

(* 构建或重建 CFG: 重置、移除无用块、连接 preds/succs *)
let build_cfg blocks : ir_block list =
  (* 1. reset *)
  reset_preds_succs blocks;
  (* 3. rebuild label map *)
  let label_map = build_label_map blocks in
  (* 4. connect edges *)
  List.iter (fun blk ->
    let add_edge target_label =
      match StringMap.find_opt target_label label_map with
      | Some succ_blk ->
        blk.succs <- target_label :: blk.succs;
        succ_blk.preds <- blk.label :: succ_blk.preds
      | None -> ()
    in
    match blk.terminator with
    | TermGoto l -> add_edge l
    | TermIf (_, l1, l2) -> add_edge l1; add_edge l2
    | TermRet _ -> () |  TermSeq l-> add_edge l
  ) blocks;
  blocks

(* 先序遍历 CFG，从 entry_label 开始 *)
let traverse_preorder blocks ~entry_label =
  let label_map = build_label_map blocks in
  let visited = Hashtbl.create (List.length blocks) in
  let order = ref [] in
  let rec dfs lbl =
    if not (Hashtbl.mem visited lbl) then begin
      Hashtbl.add visited lbl true;
      order := lbl :: !order;
      match StringMap.find_opt lbl label_map with
      | Some blk -> List.iter dfs blk.succs
      | None -> ()
    end
  in
  dfs entry_label;
  List.rev !order

(* 后序遍历，返回先序列表的反序 *)
let traverse_postorder blocks ~entry_label =
  List.rev (traverse_preorder blocks ~entry_label)
