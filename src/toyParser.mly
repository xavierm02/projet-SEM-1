
%{
  open ToyTypes

%}

%token Token_LPar Token_RPar Token_Begin Token_End

%token <int> Token_Num
%token <string> Token_Var
%token <string> Token_String

%token Token_Skip
%token Token_Assign
%token Token_Seq
%token Token_If Token_Then Token_Else
%token Token_While Token_For Token_To Token_Do
%token Token_Print
%token Token_Try Token_With Token_Arrow
%token Token_Raise
%token Token_Parse
%token Token_Eval

%token Token_Incr Token_Decr
%token Token_EAssign

%token Token_Cons
%token Token_Escape
%token Token_Unescape

%token Token_Plus Token_Minus
%token Token_Mult Token_Div
%token Token_Not
%token Token_And
%token Token_Or
%token Token_Equal Token_NotEqual
%token Token_Less Token_LessEqual Token_Greater Token_GreaterEqual

%right Token_EAssign
%left Token_Or
%left Token_And
%nonassoc Token_Not
%nonassoc Token_Less Token_LessEqual Token_Greater Token_GreaterEqual Token_Equal Token_NotEqual
%left Token_Plus Token_Minus
%left Token_Mult Token_Div
%nonassoc UnarySign

%token Token_EOP

%start make_prog             /* Entry point */

%type  <ToyTypes.prog> make_prog

%%

make_prog:
  | prog2 Token_EOP                      {$1}

prog2:
  | prog2 Token_Seq prog                 {Seq($1,$3)}
  | prog                                 {$1}

prog:
  | Token_LPar prog2 Token_RPar          {$2}
  | Token_Begin prog2 Token_End          {$2}

  | Token_Skip                           {Skip}

  | Token_Var Token_Assign expr          {Assign(Var($1),$3)}

  | Token_While expr
      Token_Do prog2 Token_End
                                         {While($2,$4)}

  | Token_For Token_Var Token_Equal
      expr Token_To expr
      Token_Do prog2 Token_End
                                         {For(Var $2, $4, $6, $8)}

  | Token_If expr
      Token_Then prog2
      Token_Else prog2 Token_End
                                         {If($2,$4,$6)}
  | Token_If expr
      Token_Then prog2 Token_End
                                         {If($2,$4,Skip)}
  | Token_Print expr                     {Print($2)}

  | Token_Try prog2
      Token_With Token_Var
      Token_Arrow prog2 Token_End
                                         {Try($2,Label($4),$6)}

  | Token_Raise Token_Var                {Raise($2)}
  | Token_Eval expr                 {Eval($2)}

expr:
  | expr Token_And expr              {Expr_And($1, $3)}
  | expr Token_Or  expr              {Expr_Or($1, $3)}
  | expr Token_Equal expr            {Expr_Equal($1,$3)}
  | expr Token_NotEqual expr         {Expr_NotEqual($1,$3)}
  | expr Token_Less expr             {Expr_Less($1,$3)}
  | expr Token_LessEqual expr        {Expr_LessEqual($1,$3)}
  | expr Token_Greater expr          {Expr_Greater($1,$3)}
  | expr Token_GreaterEqual expr     {Expr_GreaterEqual($1,$3)}
  | expr Token_Plus expr             {Expr_Plus($1,$3)}
  | expr Token_Minus expr            {Expr_Minus($1,$3)} 
  | expr Token_Mult expr             {Expr_Mult($1,$3)}
  | expr Token_Div expr              {Expr_Div($1,$3)}
  | Token_Not expr                   {Expr_Not($2)}
  | Token_Plus  expr %prec UnarySign {$2}
  | Token_Minus expr %prec UnarySign {$2}
  | Token_Num                        {Expr_Num($1)}
  | Token_Var                        {Expr_Var(Var $1)}
  | Token_Var Token_Incr             {Expr_PostPlus(Var $1)} 
  | Token_Var Token_Decr             {Expr_PostMinus(Var $1)} 
  | Token_Incr Token_Var             {Expr_PrePlus(Var $2)} 
  | Token_Decr Token_Var             {Expr_PreMinus(Var $2)} 
  | Token_Var Token_EAssign expr     {Expr_EAssign(Var $1,$3)}
  | Token_LPar expr Token_RPar       {$2}
  | Token_String                     {Expr_String(Scanf.unescaped(String.sub $1 1 ((String.length $1) - 2)))}
  | Token_Parse Token_LPar expr Token_RPar                {Expr_Parse($3)}
  | expr Token_Cons expr             {Expr_Cons($1, $3)}
  | Token_Escape Token_LPar expr Token_RPar               {Expr_Escape($3)}
  | Token_Unescape Token_LPar expr Token_RPar               {Expr_Unescape($3)}
%%
