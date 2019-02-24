(* Representations of tokens and instructions *)

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
                    (LOAD_F (bits_of_float (float_of_string s)))
                  else (LOAD_I (of_int (int_of_string s)))
  (* TODO: implement using symbol table? *)
  | (OP s)     -> (match s with
                     "+"   -> (INSTRN '\001')
                   | "-"   -> (INSTRN '\002')
                   | "*"   -> (INSTRN '\003')
                   | "/"   -> (INSTRN '\004')
                   | "^"   -> (INSTRN '\005')
                   | "NEG" -> (INSTRN '\006')
                   | _     -> (INSTRN '\000')) (* NOP *)
  | _          -> (INSTRN '\000')

(* printable representation of an instruction *)
let str_from_instr instr=
  match instr with
    (LOAD_I i) -> Printf.sprintf "LOAD_I %#010lx" i
  | (LOAD_F f) -> Printf.sprintf "LOAD_F %#010lx" f
  | (INSTRN c) -> Printf.sprintf "INSTRN %#06x" (int_of_char c)
