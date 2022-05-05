# ocaml_interpreter

**Supported Grammar**

**Constants**

digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

letter ::= a...z | A...Z

int ::= [−] digit {digit}

bool ::= True | False

name ::= letter{letter | digit | _ | ´}

const ::= int | bool | name | ()

**Programs**

prog ::= coms

com ::= 
Push const | Pop int | Trace int
| Add int | Sub int | Mul int | Div int
| Equal | Lte | And | Or | Not
| Local | Global | Lookup
| Begin coms End
| If coms Else coms End
| Fun name name coms End
| Call
| Try coms End
| Switch {Case int cmds} End

coms ::= com {com}

**Values**

env ::= {name → val}

val ::= int | bool | () | name | Clo env name name coms
