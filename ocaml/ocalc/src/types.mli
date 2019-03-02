(* Header file for Types *)

type token =
  | NUMBER of string
  | OP of string
  | LPAREN of string
  | RPAREN of string

type instr =
  | LOAD_I of int
  | LOAD_F of float
  | ADD | SUB | MUL | DIV | NEG | EXP | NOP

(* printable representation of a token *)
val str_from_token : token -> string

(* convert token to instruction *)
val instr_from_token : token -> instr

(* printable representation of an instruction *)
val str_from_instr : instr -> string
