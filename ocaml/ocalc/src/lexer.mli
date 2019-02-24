(* Header file for Lexer *)

open List

type token =
  | NUMBER of string
  | OP of string
  | LPAREN of string
  | RPAREN of string

exception Failure

(* converts string to list of chars *)
val chars_from_str : string -> char list

(* converts list of chars to string *)
val str_from_chars : char list -> string

(* strip charlist of any whitespaces *)
val strip : char list -> char list

(* split list of chars into digits and rest of list (returns pair) *)
val take_number : char list -> (char list * char list)

(* generate tokens from string *)
val generate_tokens : string -> token list

(* returns true if the string has whitespace between numbers *)
val whitespace_between_nums : string -> bool

(* printable representation of a token *)
val str_from_token : token -> string

(* tokenise and concatenate all tokens into string *)
val print_all_tokens : string -> string

(* tokenise user input *)
val lex : string -> string
