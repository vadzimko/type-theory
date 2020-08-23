{
open Parser
exception EOF
}

let var_reg = ['a' - 'z'] + ['a'-'z' '0' - '9' '\'']*
let empty = [' ' '\t' '\r' '\n']

rule main = parse
        var_reg as var       { VAR(var) }
        | "("empty*          { LPAREN }
        | empty*")"          { RPAREN }
        | empty+             { APPLY }
        | empty*'.'empty*    { DOT }
        | "\\"empty*         { LAMBDA }
        | eof                { EOF }
