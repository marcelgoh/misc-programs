(* Functions to parse input *)

type token =
  | Num of int
  | Dec of float
  | Str of string

(* converts string to list of chars *)
let str_to_chars str =
  (* build list backwards from last char in string *)
  let rec iter i cs =
    if i < 0 then cs else iter (i - 1) (str.[i] :: cs) in
  iter (String.length str - 1) []

(* converts list of chars to string *)
let chars_to_str chars =
  String.concat "" (List.map (String.make 1) chars)

let lex str =
  chars_to_str (str_to_chars str)

let parse str = lex str
