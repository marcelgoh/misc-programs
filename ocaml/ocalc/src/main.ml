(* Command-line interface of the program *)

open Printf

open Lexer
open Parser
open Types

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
  | ":help" -> printf "I don't do much at the moment.\n";
               loop ()
  | _       -> let tokens = try lex input with Lex_fail -> printf "Invalid input."; [] in
               let parsed = shunt tokens in
               let asm_out = String.concat "\n"
                               (List.map (fun x -> str_from_instr (instr_from_token x)) parsed) in
               printf "%s\n" asm_out;
               loop ()

(* print startup blurb and start loop *)
let main () =
  printf "+----------------------------------------------+\n";
  printf "| OCalc Interactive Calculator                 |\n";
  printf "| Author: Marcel Goh (Release: 24 Feb '19)     |\n";
  printf "| Type \":help\" for help or \":q\" to quit.       |\n";
  printf "+----------------------------------------------+\n";
  loop ()

let _ = main ()
