set_option autoImplicit false

def hello := "world"

abbrev ParserType s t := List s -> List (Prod t (List s))

inductive Parser (s t : Type) : Type :=
  | P : ParserType s t -> Parser s t

def pLettera : Parser Char Char :=
  Parser.P (λ inp =>
    match inp with
    | s :: ss => if s == 'a' then [('a', ss)] else []
    | _ => [])

def pSym {s : Type} [DecidableEq s] (a : s) : Parser s s :=
  Parser.P (λ inp =>
    match inp with
    | s :: ss => if a = s then [(s, ss)] else []
    | _ => [])

inductive Token : Type :=
  | Id : Token -- | Terminal Symbol
  | Ident : String -> Token -- | Token contains a String Identifier
  | Number : Nat -> Token -- | Token contains an Int value
  | If_Symbol : Token
  | Then_Symbol : Token
deriving DecidableEq, BEq

 -- | Basic Building blocks of the parser combinator
def pIdent := pSym Token.Id
def pReturn {s t : Type} (a : t) : Parser s t := Parser.P (λ inp => [(a, inp)])
def pFail {s t : Type} : Parser s t := Parser.P (λ _ => [])
