(* Functions to parse input *)

open List

type token =
  | NUMBER of string
  | OP of string
  | LPAREN of string
  | RPAREN of string

exception Failure

(* converts string to list of chars *)
let chars_from_str str =
  (* build list backwards from last char in string *)
  let rec iter i cs =
    if i < 0 then cs else iter (i - 1) (str.[i] :: cs) in
  iter (String.length str - 1) []

(* converts list of chars to string *)
let str_from_chars chars =
  String.concat "" (map (String.make 1) chars)

(* strip charlist of any whitespaces *)
let strip chars =
  let rec iter acc left =
    match left with
      []    -> rev acc
    | c::cs -> if mem c [' ';'\r';'\t';'\n';'\x0c'] then
                 iter acc cs
               else iter (c::acc) cs in
  iter [] chars

(* split list of chars into digits and rest of list (returns pair) *)
let take_number chars =
  let rec iter acc left decimal_found =
    match left with
      []    -> (rev acc, [])
    | c::cs -> if (c >= '0' && c <= '9') then
                 iter (c::acc) cs decimal_found
               else if c == '.' && not decimal_found then
                      (* accept a decimal only once *)
                      iter (c::acc) cs true
                    else
                      (rev acc, left) in
  let ret_list = iter [] chars false in
  if length (fst ret_list) == 1 && hd (fst ret_list) == '.' then
    raise Failure
  else ret_list

(* generate tokens from charlist *)
let generate_tokens str =
  let chars = strip (chars_from_str str) in
  let rec iter tokens left =
    match left with
      []      -> rev tokens
    | '('::cs -> iter ((LPAREN "(")::tokens) cs
    | ')'::cs -> iter ((RPAREN ")")::tokens) cs
    | '-'::cs -> (match (hd tokens) with
                    (LPAREN _)
                  | (OP _) -> iter ((OP "NEG")::tokens) cs
                  | _      -> iter ((OP "-")::tokens) cs)
    | '.'::cs -> (match (hd tokens) with
                    (NUMBER _) -> raise Failure
                  | _          -> let pair = take_number left in
                                  let s = (str_from_chars (fst pair)) in
                                  iter ((NUMBER s)::tokens) (snd pair))
    | c::cs   -> let ops_str = "+*/^" in
                 if String.contains ops_str c then
                   let s = String.make 1 c in
                   iter ((OP s)::tokens) cs
                 else if (c >= '0' && c <= '9') then
                        let pair = take_number left in
                        let s = (str_from_chars (fst pair)) in
                        iter ((NUMBER s)::tokens) (snd pair)
                 else raise Failure in
  iter [] chars

(* printable representation of a token *)
let str_from_token tok =
  let pair =
    match tok with
      (LPAREN s) -> ("LPAREN", s)
    | (RPAREN s) -> ("RPAREN", s)
    | (NUMBER s) -> ("NUMBER", s)
    | (OP s)     -> ("OP", s) in
  Printf.sprintf "%s %s" (fst pair) (snd pair)

(* returns true if the string has whitespace between numbers *)
let whitespace_between_nums str =
  let regex = Str.regexp "[0-9.][ \r\t\n\x0c]+[0-9.]" in
  try let _ = Str.search_forward regex str 0 in
        true
  with Not_found -> false

let print_all_tokens str =
  String.concat "\n" (map str_from_token (generate_tokens str))

let lex str = if whitespace_between_nums str then
                raise Failure
              else print_all_tokens str
