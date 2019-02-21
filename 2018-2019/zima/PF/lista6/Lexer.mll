{ open Parser }

let whitespace = ['\011'-'\r' '\n' '\t' ' ']

let name_char = ['a'-'z' '_']
let var_char  = ['A'-'Z']

rule token = parse
    whitespace+     {token lexbuf}
  | "Goal"          { KW_GOAL }
  | ":"             { COLON }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "("             { LPAR }
  | ")"             { RPAR }
  | "["             { LBR }
  | "]"             { RBR }
  | "∧" | "/\\"     { CONJ }
  | "∨" | "\\/"     { DISJ }
  | "⇒" | "=>"      { IMPL }
  | "ImplI"         {KW_II}
  | "ImplE"         {KW_IE}
  | "ConjI"         {KW_CI}
  | "ConjEL"        {KW_CE1}
  | "ConjER"        {KW_CE2}
  | "DisjIL"        {KW_DI1}
  | "DisjIR"        {KW_DI2}
  | "DisjE"         {KW_DE}
  | "TopI"          {KW_TI}
  | "BotE"          {KW_FE}
  | "Ax"            {KW_AX}
  | "⊤" | "True"    { TOP }
  | "⊥" | "False"   { BOT }
  | "="             { EQ }
  | name_char+ as x { NAME(x) }
  | var_char+  as x { VAR(x)  }
  | eof             { EOF }
