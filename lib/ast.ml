(* 基本类型定义 *)
type typ =
  | TInt (* 整数类型 *)
  | TVoid (* 空类型 *)

(* 一元运算符 *)
type unary_op =
  | Pos (* 正号 + *)
  | Neg (* 负号 - *)
  | Not (* 逻辑非 ! *)

(* 关系运算符 *)
type rel_op =
  | Eq (* == *)
  | Ne (* != *)
  | Lt (* < *)
  | Gt (* > *)
  | Le (* <= *)
  | Ge (* >= *)

(* 表达式 *)
type expr =
  | LOr of expr * expr (* 逻辑或 || *)
  | LAnd of expr * expr (* 逻辑与 && *)
  | RelBinary of expr * rel_op * expr (* 关系运算 *)
  | Add of expr * expr (* 加法 + *)
  | Sub of expr * expr (* 减法 - *)
  | Mul of expr * expr (* 乘法 * *)
  | Div of expr * expr (* 除法 / *)
  | Mod of expr * expr (* 取模 % *)
  | UnaryOp of unary_op * expr (* 一元运算 *)
  | ID of string (* 标识符 *)
  | Number of int (* 整数常量 *)
  | Call of string * expr list (* 函数调用 *)

(* 语句 *)
type stmt =
  | Block of stmt list (* 语句块 {...} *)
  | Empty (* 空语句 ; *)
  | ExprStmt of expr (* 表达式语句 *)
  | Decl of string * expr option (* 变量声明（带or不带初始化） *)
  | Assign of string * expr (* 赋值语句 *)
  | If of expr * stmt * stmt option (* if语句（else可选） *)
  | While of expr * stmt (* while循环 *)
  | Break (* break语句 *)
  | Continue (* continue语句 *)
  | Return of expr option (* return语句（返回值可选） *)

(* 函数参数 *)
type param = string (* 参数名（类型固定为int） *)

(* 函数定义 *)
type func_def =
  { ret_type : typ (* 返回类型 *)
  ; func_name : string (* 函数名 *)
  ; params : param list (* 参数列表 *)
  ; body : stmt list (* 函数体（语句列表） *)
  }

(* 编译单元（整个程序） *)
type comp_unit = func_def list (* 函数定义列表 *)
