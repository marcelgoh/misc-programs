(* Functions to parse tokens into instructions *)

open Types

exception Parse_fail

(* returns an integer representing the operator precedence *)
let precedence tok =
  match tok with
    (OP s) -> if s = "^" || s = "NEG" then 4
              else if s = "*" || s = "/" then 3
                   else if s = "-" || s = "+" then 2 else raise Parse_fail
  | _ -> raise Parse_fail

(* returns true if left-associative, false otherwise *)
let left_assoc tok =
  match tok with
    (OP s) -> if s = "^" || s = "NEG" then false else true
  | _ -> raise Parse_fail

(* use shunting-yard algorithm to parse into reverse-polish order *)
let shunt tok_list =
  (* combine the operator and output stacks into one queue *)
  let rec combine out op =
    match op with
      []    -> List.rev out
    | t::ts -> (match t with
                  (LPAREN _)
                | (RPAREN _) -> raise Parse_fail (* mismatched parentheses *)
                | _          -> combine (t::out) ts) in
  (* inserts operator into correct spot *)
  let rec handle_op out op t =
    match op with
      []    -> (out, t::op)
    | o::os -> (match o with
                  (RPAREN _) -> handle_op (o::out) os t
                | (OP _)     -> if precedence o > precedence t ||
                                   precedence o = precedence t && left_assoc t then
                                   handle_op (o::out) os t
                                else (out, t::op)
                | (LPAREN _) -> (out, t::op)
                | _          -> raise Parse_fail) in (* number in operator stack *)
  (* pops operators onto output stack until matching left parenthesis found *)
  let rec handle_rparen out op =
    match op with
      (* failed to find matching left parenthesis *)
      []    -> raise Parse_fail
    | o::os -> (match o with
                  (OP _)     -> handle_rparen (o::out) os
                | (LPAREN _) -> (out, os)
                | _          -> raise Parse_fail) in (* number/rparen in operator stack *)
  (* loop through token-list *)
  let rec iter out op left =
    match left with
      []    -> combine out op
    | t::ts -> (match t with
                  (NUMBER _) -> iter (t::out) op ts
                | (OP _)     -> let pair = handle_op out op t in
                                iter (fst pair) (snd pair) ts
                | (LPAREN _) -> iter out (t::op) ts
                | (RPAREN _) -> let pair = handle_rparen out op in
                                iter (fst pair) (snd pair) ts) in
  iter [] [] tok_list
