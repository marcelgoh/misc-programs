(* Representations of tokens and instructions *)

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
let str_from_token tok =
  let pair =
    match tok with
      (LPAREN s) -> ("LPAREN", s)
    | (RPAREN s) -> ("RPAREN", s)
    | (NUMBER s) -> ("NUMBER", s)
    | (OP s)     -> ("OP", s) in
  Printf.sprintf "%s %s" (fst pair) (snd pair)

(* convert token to instruction *)
let instr_from_token tok =
  match tok with
    (NUMBER s) -> if String.contains s '.' then
                    (LOAD_F (float_of_string s))
                  else (LOAD_I (int_of_string s))
  | (OP s)     -> (match s with
                     "+"   -> ADD
                   | "-"   -> SUB
                   | "*"   -> MUL
                   | "/"   -> DIV
                   | "^"   -> EXP
                   | "NEG" -> NEG
                   | _     -> NOP)
  | _          -> NOP

(* printable representation of an instruction *)
let str_from_instr instr=
  match instr with
    (LOAD_I i) -> Printf.sprintf "LOAD_INT\t%d" i
  | (LOAD_F f) -> Printf.sprintf "LOAD_FLT\t%f" f
  | ADD -> Printf.sprintf "ADD"
  | SUB -> Printf.sprintf "SUBTRACT"
  | MUL -> Printf.sprintf "MULTIPLY"
  | DIV -> Printf.sprintf "DIVIDE"
  | NEG -> Printf.sprintf "NEGATE"
  | EXP -> Printf.sprintf "EXPONENT"
  | NOP -> Printf.sprintf "NO OPERATION"
