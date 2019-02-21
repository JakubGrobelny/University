%token<string> VAR NAME
%token EQ COLON SEMICOLON LPAR RPAR LBR RBR COMMA
%token CONJ DISJ IMPL TOP BOT
%token KW_GOAL
%token KW_CI KW_CE1 KW_CE2 KW_DI1 KW_DI2 KW_DE KW_II KW_IE KW_TI KW_FE KW_AX
%token EOF

%type<Syntax.file> file
%start file

%right SEMICOLON
%right IMPL
%left DISJ
%left CONJ


%{
    open Syntax

%}

%%

file :
  def_list EOF { $1 }
;

def_list :
    { [] }
  | def def_list { $1 :: $2 }
;

def :
    KW_GOAL NAME COLON prop EQ pf { SGoal ($2, $4, $6) }
  | KW_GOAL NAME COLON prop EQ pt { TGoal ($2, $4, $6) }

;

pt :
    KW_AX  LPAR prop RPAR          { Ax $3 }
  | KW_TI                          { TopI }
  | KW_CI  LPAR pt COMMA pt   RPAR { ConjI  ($3, $5) }
  | KW_DI1 LPAR pt COMMA prop RPAR { DisjIL ($3, $5) }
  | KW_DI2 LPAR prop COMMA pt RPAR { DisjIR ($3, $5) }
  | KW_II  LPAR thyp RPAR          { ImplI  $3 }
  | KW_FE  LPAR prop RPAR          { BotE   $3 }
  | KW_CE1 LPAR pt RPAR            { ConjEL $3 }
  | KW_CE2 LPAR pt RPAR            { ConjER $3 }
  | KW_DE  LPAR pt COMMA thyp COMMA thyp RPAR { DisjE ($3, $5, $7) }
  | KW_IE  LPAR pt COMMA pt RPAR   { ImplE  ($3, $5) }

thyp :
    LBR prop COLON pt RBR { ($2, $4) }

pf :
    phyp SEMICOLON pf { PHyp ($1, $3) }
  | prop SEMICOLON pf { PConc ($1, $3) }
  | prop              { PDone $1 }
;

phyp :
  LBR prop COLON pf RBR { ($2, $4) }
;

prop :
    VAR            { Var $1 }
  | TOP            { Top }
  | BOT            { Bot }
  | prop CONJ prop { Conj ($1, $3) }
  | prop DISJ prop { Disj ($1, $3) }
  | prop IMPL prop { Impl ($1, $3) }
  | LPAR prop RPAR { $2 }
