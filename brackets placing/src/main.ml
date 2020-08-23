open Tree;;
open Buffer;;
open Pervasives;;
open Printf;;

(*let cin = open_in "input.txt";;*)
let cout = stdout;;
let cin =  stdin;;

let rec parse_tree tree str = match tree with
      Var var                   -> add_string str var

    | Apply (left,right)        -> add_char str '(';
                                   parse_tree left str;
                                   add_char str ' ';
                                   parse_tree right str;
                                   add_char str ')'

    | Abstract (var,arg)        -> add_string str "(\\";
                                   add_string str var;
                                   add_char str '.';
                                   parse_tree arg str;
                                   add_char str ')';;

let tree_to_string tree =
    let str = Buffer.create 10000 in
    parse_tree tree str;
    contents str;;

let read_input () = begin
  let lines = ref [] in
  let read_lines () =
    try
      while true; do
        let line = input_line cin in
        lines := line :: !lines
        done; !lines
      with _ -> List.rev !lines in
  let str = Buffer.create 10000 in
  let rec join lines = match lines with
  | h::t ->  add_string str h;
             add_char str '\n';
             join t

  | [] -> contents str in
  join (read_lines());
end;;

let input = read_input();;
let input = Str.global_replace (Str.regexp "\\\\") " \\\\" input;;
let input = Str.global_replace (Str.regexp "(") " ( " input;;
let input = Str.global_replace (Str.regexp ")") " ) " input;;
let input = String.trim input;;
(*fprintf cout "%s\n" input;;*)

try
    fprintf cout "%s" (tree_to_string (Parser.main Lexer.main (Lexing.from_string input)))
with _ -> exit 0

