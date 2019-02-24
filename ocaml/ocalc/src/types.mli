(* Header file for Types *)

open Int32

type token =
  | NUMBER of string
  | OP of string
  | LPAREN of string
  | RPAREN of string

type instr =
  | LOAD_I of int32
  | LOAD_F of int32
  | INSTRN of char

(* printable representation of a token *)
val str_from_token : token -> string

(* convert token to instruction *)
val instr_from_token : token -> instr

(* printable representation of an instruction *)
val str_from_instr : instr -> string
