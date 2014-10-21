{
  (** Description de l'analyseur lexical TOY *)
  open ToyParser
}

let BLANK = [' ' '\t' '\n']

let LINE = [^ '\n']* '\n'
let COMMENT = '#' LINE

let NUM = ['0'-'9']
let ALPHA =  ['a'-'z' 'A'-'Z' '_' ]
let WORD = ALPHA (ALPHA | '-' | NUM)*
let NUMBER = '-'? ['0'-'9']+
let STRING = '"' ([^'"'] | "\\\"")* '"'

let ANY = _

let LPAR = "("
let RPAR = ")"
let BEGIN = "begin"
let END = "end"

let SKIP = "skip"
let SEQ = ";"
let WHILE = "while"
let FOR = "for"
let TO = "to"
let DO = "do"
let IF = "if"
let THEN = "then"
let ELSE = "else"
let ASSIGN = ":="
let PRINT = "print"
let TRY = "try"
let WITH = "with"
let RAISE = "raise"
let ARROW = "->"
let PARSE = "parse"
let EVAL = "eval"

let PLUS = "+"
let MINUS = "-"
let TIMES = "*"
let DIV = "/"

let NOT = "!" | "~"
let AND = "&&"
let OR  = "||"

let EQUAL = "="
let NEQ   = "<>" | "!="
let LESS  = "<"
let LESSE = "<="
let GREAT = ">"
let GREATE= ">="

let INCR = "++"
let DECR = "--"
let EASSIGN = "<-"

let EOP = "."

rule make_token = parse

  | BLANK               {make_token lexbuf}     (* Skip blanks and comments*)
  | COMMENT             {make_token lexbuf}

  | eof                 {Token_EOP}     (* Give up on end of file *)

  | EOP                 {Token_EOP}

  | LPAR                {Token_LPar}
  | RPAR                {Token_RPar}
  | BEGIN               {Token_Begin}
  | END                 {Token_End}

  | SKIP                {Token_Skip}
  | SEQ                 {Token_Seq}
  | WHILE               {Token_While}
  | FOR                 {Token_For}
  | TO                  {Token_To}
  | DO                  {Token_Do}
  | IF                  {Token_If}
  | THEN                {Token_Then}
  | ELSE                {Token_Else}
  | ASSIGN              {Token_Assign}
  | PRINT               {Token_Print}
  | TRY                 {Token_Try}
  | WITH                {Token_With}
  | ARROW               {Token_Arrow}
  | RAISE               {Token_Raise}
  | PARSE               {Token_Parse}
  | EVAL                {Token_Eval}
  
  | PLUS                {Token_Plus}
  | MINUS               {Token_Minus}
  | TIMES               {Token_Mult}
  | DIV                 {Token_Div}
  | NOT                 {Token_Not}
  | AND                 {Token_And}
  | OR                  {Token_Or}
  | EQUAL               {Token_Equal}
  | NEQ                 {Token_NotEqual}
  | LESS                {Token_Less}
  | LESSE               {Token_LessEqual}
  | GREAT               {Token_Greater}
  | GREATE              {Token_GreaterEqual}

  | INCR                {Token_Incr}
  | DECR                {Token_Decr}
  | EASSIGN             {Token_EAssign}

  | NUMBER
    {
      let s = (Lexing.lexeme lexbuf)
      in Token_Num(int_of_string(s))
    }

  | WORD
      {
        let s = (Lexing.lexeme lexbuf)
        in Token_Var(s)
      }
  
  | STRING
      {
        let s = (Lexing.lexeme lexbuf) in
        Token_String(s)
      }
  | ANY (* Default case: just skip the character *)
      {
        let s = (Lexing.lexeme lexbuf)
        in (print_string ("Bad char: " ^ s ^ ". Continuing...\n");
            make_token lexbuf)
      }
