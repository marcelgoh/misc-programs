(* Functions to evaluate instruction list *)

open Types

exception Eval_fail

(* returns float operation corresponding with instruction *)
let intop_from_inst i =
  match i with
    ADD -> (+)
  | SUB -> (-)
  | MUL -> ( * )
  | DIV -> (/)
  | _   -> raise Eval_fail

(* returns int operation corresponding with instruction *)
let fltop_from_inst i =
  match i with
    ADD -> (+.)
  | SUB -> (-.)
  | MUL -> ( *. )
  | DIV -> (/.)
  | _   -> raise Eval_fail

(* evaluate a binary operation *)
(* arguments are evaluated in reverse order because of the nature of the stack *)
let bineval ins arg1 arg2 =
  match ins with
    (* exponentiation always casts to float *)
    EXP -> (match arg1 with
              LOAD_I i1 -> let c1 = float_of_int i1 in
                             (match arg2 with
                                LOAD_I i2 -> let c2 = float_of_int i2 in
                                           LOAD_F (c2 ** c1)
                              | LOAD_F f2 -> LOAD_F (f2 ** c1)
                              | _         -> raise Eval_fail)
            | LOAD_F f1 -> (match arg2 with
                              LOAD_I i2 -> let c2 = float_of_int i2 in
                                           LOAD_F (c2 ** f1)
                            | LOAD_F f2 -> LOAD_F (f2 ** f1)
                            | _         -> raise Eval_fail)
            | _         -> raise Eval_fail)
  | _   -> (match arg1 with
              LOAD_I i1 -> (match arg2 with
                              LOAD_I i2 -> let op = intop_from_inst ins in
                                           LOAD_I (op i2 i1)
                            | LOAD_F f2 -> let op = fltop_from_inst ins in
                                           LOAD_F (op f2 (float_of_int i1))
                            | _         -> raise Eval_fail)
            | LOAD_F f1 -> (match arg2 with
                              LOAD_I i2 -> let op = fltop_from_inst ins in
                                           LOAD_F (op (float_of_int i2) f1)
                            | LOAD_F f2 -> let op = fltop_from_inst ins in
                                           LOAD_F (op f2 f1)
                            | _         -> raise Eval_fail)
            | _         -> raise Eval_fail)

(* reduce instruction stack to single instruction *)
let follow instrs =
  let rec iter out ins =
        match ins with
          []    -> out     (* return contents of output stack *)
        | i::is -> (match i with
                      ADD | SUB | MUL | DIV | EXP
                          -> (match out with
                                a::b::rest -> let new_inst = bineval i a b in
                                              iter (new_inst::rest) is
                              | _ -> raise Eval_fail)
                    | NEG -> (match out with
                                a::rest -> (match a with
                                              LOAD_I n -> iter ((LOAD_I (- n))::rest) is
                                            | LOAD_F n -> iter ((LOAD_F (-. n))::rest) is
                                            | _        -> raise Eval_fail)
                              | _       -> raise Eval_fail)
                    | NOP -> iter out is
                    | _   -> iter (i::out) is) in
  iter [] instrs

(* return answer as string *)
let eval instrs =
  let result = follow instrs in
  match result with
    a::[] -> (match a with
                LOAD_I i -> Printf.sprintf "%d" i
              | LOAD_F f -> Printf.sprintf "%f" f
              | _        -> raise Eval_fail)
  | _    -> raise Eval_fail
