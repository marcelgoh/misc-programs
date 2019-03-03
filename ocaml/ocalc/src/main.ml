(* Command-line interface of the program *)

open Printf

open Lexer
open Parser
open Types
open Eval

(* exit the program *)
let quit () =
  printf "Ich sterbe.\n";
  exit 0

(* main loop *)
let rec loop () =
  printf "]=> ";
  let input = read_line () in
  match input with
    ":q"    -> quit ()
  | ":help" -> printf "%s%s%s%s%s%s%s%s%s%s\n"
                      "Type a mathematical expression to have it evaluated!\n"
                      "Supported operators:\n"
                      "    (+) Addition\n"
                      "    (-) Subtraction/Unary negation\n"
                      "    (*) Multiplication\n"
                      "    (/) Division\n"
                      "    (^) Exponentiation\n"
                      "Use brackets to force operator precedence.\n"
                      "Note: Complex numbers not supported. Be careful when\n"
                      "taking negative numbers to fractional exponents.";
               loop ()
  | _       -> let tokens = try lex input with Lex_fail -> [] in
               let instrs = try List.map instr_from_token (shunt tokens)
                              with Parse_fail -> [] in
               let asm_out = if List.length instrs == 0 then
                               "NIL"
                             else String.concat "\n    " (List.map str_from_instr instrs) in
               let answer = try eval instrs with Eval_fail -> "Invalid input." in
               printf "Instructions:\n    %s\n" asm_out;
               printf "Answer:\n    %s\n" answer;
               loop ()

(* print startup blurb and start loop *)
let main () =
  printf "+----------------------------------------------+\n";
  printf "|         OCALC INTERACTIVE CALCULATOR         |\n";
  printf "|   Author: Marcel Goh (Release: 3 Mar '19)    |\n";
  printf "|    Type \":help\" for help or \":q\" to quit.    |\n";
  printf "+----------------------------------------------+\n";
  loop ()

let _ = main ()
