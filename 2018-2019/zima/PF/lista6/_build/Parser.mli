type token =
  | VAR of (string)
  | NAME of (string)
  | EQ
  | COLON
  | SEMICOLON
  | LPAR
  | RPAR
  | LBR
  | RBR
  | COMMA
  | CONJ
  | DISJ
  | IMPL
  | TOP
  | BOT
  | KW_GOAL
  | KW_CI
  | KW_CE1
  | KW_CE2
  | KW_DI1
  | KW_DI2
  | KW_DE
  | KW_II
  | KW_IE
  | KW_TI
  | KW_FE
  | KW_AX
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.file
