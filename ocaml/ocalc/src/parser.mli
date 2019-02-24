(* Header for Parser *)

open Types

exception Parse_fail

(* returns an integer representing the operator precedence *)
val precedence : token -> int

(* returns true if left-associative, false otherwise *)
val left_assoc : token -> bool

(* use shunting-yard algorithm to parse into reverse-polish order *)
val shunt : token list -> token list
