type expr =
  | Var of string
  | Float of float
  | Dir of string
  | String of string
  | Mult of expr * expr
  | Div of expr * expr
  | Add of expr * expr
  | Sub of expr * expr

type matrix = expr option array array

type command =
  | Set of string * string
  | Let of string * string list * matrix
