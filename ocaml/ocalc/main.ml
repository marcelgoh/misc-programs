(* Command-line interface of the program *)

open Printf
open Lexer

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
  | _       -> let output = try lex input with Failure -> "Invalid input." in
               printf "%s\n" output;
               loop ()

(* print startup blurb and start loop *)
let main () =
  printf "+----------------------------------------------+\n";
  printf "| OCalc Interactive Calculator                 |\n";
  printf "| Author: Marcel Goh (Release: 22 Feb '19)     |\n";
  printf "| Type \":help\" for help or \":q\" to quit.       |\n";
  printf "+----------------------------------------------+\n";
  loop ()

let _ = main ()
