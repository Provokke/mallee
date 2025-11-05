type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr

type toplevel =
  | Def of string * expr
  | Expr of expr

