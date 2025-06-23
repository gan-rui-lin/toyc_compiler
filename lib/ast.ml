type comp_unit = func_def list

and func_def =
  { ret_type : typ (* "int" or "void" *)
  ; func_name : string
  ; params : param list
  ; body : block
  }

and typ =
  | TInt
  | TVoid

and param = string (* 只支持 int ID *)
and block = stmt list

and stmt =
  | Block of block
  | ExprStmt of expr (* Expr ";" *)
  | Decl of string * expr option (* int ID 或 int ID = Expr *)
  | Assign of string * expr (* ID = Expr *)
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option

and expr = lor_expr

and lor_expr =
  | LOr of lor_expr * land_expr
  | LAndExpr of land_expr

and land_expr =
  | LAnd of land_expr * rel_expr
  | RelExpr of rel_expr

and rel_expr =
  | RelBinary of rel_expr * relop * add_expr
  | AddExpr of add_expr

and relop =
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Ne

and add_expr =
  | Add of add_expr * mul_expr
  | Sub of add_expr * mul_expr
  | MulExpr of mul_expr

and mul_expr =
  | Mul of mul_expr * unary_expr
  | Div of mul_expr * unary_expr
  | Mod of mul_expr * unary_expr
  | UnaryExpr of unary_expr

and unary_expr =
  | UnaryOp of unary_op * unary_expr
  | Primary of primary_expr

and unary_op =
  | Not (* ! *)
  | Neg (* - *)
  | Pos (* + *)

and primary_expr =
  | ID of string
  | Number of int
  | Expr of expr (* (Expr) *)
  | Call of string * expr list
(* ID("(" Expr ("," Expr)* ")") *)


(* 树状打印表达式类型
let rec string_of_expr (e : expr) : string = 
  match e with
  | Int n -> Printf.sprintf "Int %d" n
  | Bool b -> Printf.sprintf "Bool %b" b
  | Var x -> Printf.sprintf "Var %s" x
  | Unop (unop, e) -> 
    let uop_str = 
      match unop with
      | Not -> "Not" 
      | Minus -> "Minus"
    in
    Printf.sprintf "Unary %s %s" uop_str (string_of_expr e)
  | Binop (binop, e1, e2) ->
    let binop_str = 
      match binop with 
      | Add -> "Add"
      | Mul -> "Mul"
      | Sub -> "Sub"
      | Div -> "Div"
      | Leq -> "Leq"
      | Greater -> "Greater"
      | Less -> "Less"
      | Eq -> "Eq"
      | Neq -> "Neq"
      | Land -> "Land"
      | Lor -> "Lor"
    in
    Printf.sprintf "Binop (%s, %s, %s)" binop_str (string_of_expr e1) (string_of_expr e2)
  | Let (e1, e2, e3) -> Printf.sprintf "Let (%s, %s, %s)"  (e1) (string_of_expr e2) (string_of_expr e3)
  | If (e1, e2, e3) -> Printf.sprintf "If (%s, %s, %s)"  (string_of_expr e1) (string_of_expr e2) (string_of_expr e3) *)
