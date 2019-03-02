(* Header for Eval *)

open Types

exception Eval_fail

(* returns operation corresponding with instruction in either flt or int mode *)
val intop_from_inst : instr -> (int -> int -> int)

(* returns operation corresponding with instruction in either flt or int mode *)
val fltop_from_inst : instr -> (float -> float -> float)

(* evaluate a binary operation *)
val bineval : instr -> instr -> instr -> instr

(* reduce instruction stack to (hopefully) single instruction *)
val follow : instr list -> instr list

(* return answer as string *)
val eval : instr list -> string
