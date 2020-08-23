type token =
  | VAR of (string)
  | LPAREN
  | RPAREN
  | DOT
  | LAMBDA
  | EOF
  | APPLY

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.tree
