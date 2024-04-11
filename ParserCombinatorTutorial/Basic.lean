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
  | Ident : String -> Token -- | Token contains a String Identifier
  | Number : Nat -> Token -- | Token contains an Int value
  | If_Symbol : Token
  | Then_Symbol : Token
deriving DecidableEq

@[match_pattern]
def Token.Id : Token := Token.Ident ""

-- | Helper functions
-- def const {a b : Type} (x : b) : a -> b := λ _ => x

 -- | Basic Building blocks of the parser combinator
def pIdent := pSym Token.Id
def pReturn {s t : Type} (a : t) : Parser s t := Parser.P (λ inp => [(a, inp)])
def pFail {s t : Type} : Parser s t := Parser.P <| λ _ => []

-- | <*> / applicative operator that composes 2 parsers into one

instance {s : Type} : Applicative (Parser s) where
  pure p  := Parser.P <| λ inp => [(p, inp)]
  seqLeft p₁ p₂ :=
    match p₂ () with
    | Parser.P p₂ =>
      match p₁ with
      | Parser.P p₁ => Parser.P <| λ inp =>
        p₂ inp |>.map (λ (_, ss₂) => p₁ ss₂)
               |>.head!
  seqRight p₁ p₂ :=
    match p₂ () with
    | Parser.P p₂ =>
      match p₁ with
      | Parser.P p₁ => Parser.P <| λ inp =>
        p₁ inp |>.map (λ (_, ss₂) => p₂ ss₂)
               |>.head!
  seq p₁ p₂ :=
    match p₁ with
    | Parser.P p₁ =>
      match p₂ () with
      | Parser.P p₂ => Parser.P <| λ inp =>
        p₁ inp |>.map (λ (v₁, ss₁) => (v₁, p₂ ss₁))
               |>.map (λ (v₁, lst) => lst.map (λ (v₂, ss₂) => (v₁ v₂, ss₂)))
               |>.head!

instance {s t : Type} : OrElse (Parser s t) where
  orElse := λ (Parser.P p₁) p₂ =>
  let p₂ := match p₂ () with | Parser.P p₂ => p₂
  Parser.P <| λ inp => (p₁ inp) ++ (p₂ inp)

def pString_aa :=
  (pReturn (List.cons) <*> pLettera)
  <*>
  (pReturn (λ x => [x]) <*> pLettera)

def pChoice {s t : Type} (ps : List (Parser s t)) : Parser s t := List.foldr (· <|> ·) pFail ps
