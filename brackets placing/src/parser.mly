%{
open Tree;;
%}
%token <string> VAR
%token LPAREN RPAREN
%token DOT LAMBDA
%token EOF
%token APPLY
%left APPLY
%start main
%type <Tree.tree> main
%%
main:
        tree EOF             { $1 }
tree:
        VAR                    { Var ($1) }
        | LPAREN tree RPAREN   { $2 }
        | tree APPLY tree      { Apply    ($1, $3) }
        | LAMBDA VAR DOT tree   { Abstract ($2, $4) }
