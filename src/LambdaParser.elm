module LambdaParser exposing
    ( TermLit (..)
    , parser
    , printLit
    , problem2String )       

import Parser exposing (..)
import Char

type TermLit = VarLit String
             | AppLit TermLit TermLit
             | LamLit String TermLit

-- Lexer
lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

lambda : Parser ()
lambda = lexeme <| oneOf [ symbol "\\"
                         , symbol "\u{03BB}"  -- unicode lambda
                         , symbol "\u{00A5}"  -- yen mark
                         ]

varLit : Parser String
varLit = chompIf Char.isAlpha
       |> getChompedString
       |> lexeme

-- Parser
flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

paren : Parser a -> Parser a
paren p =
    succeed identity
        |. symbol "("
        |. spaces
        |= p
        |. symbol ")"
        |. spaces
    
lamOrVar : Parser TermLit
lamOrVar =
    oneOf [ lamLit
          , Parser.map VarLit varLit
          , paren <| Parser.lazy (\() -> termLit) 
          ]
                
termLit : Parser TermLit
termLit =  lamOrVar
        |> (flip Parser.loop termLits |> andThen)

termLits : TermLit -> Parser (Step (TermLit) (TermLit))
termLits terms =
    oneOf
    [ succeed (\term -> Loop <| AppLit terms term)
    |= lamOrVar
    , succeed ()
    |> Parser.map (\_ -> Done terms)
    ]

varLits : List String -> Parser (Step (List String) (List String)) 
varLits vars =
    oneOf
        [ succeed (\var -> Loop (var::vars))
        |= varLit
        , lexeme (symbol ".")
        |> andThen (\_ -> if List.isEmpty vars then problem "0 arg"
                              else succeed <| Done (List.reverse vars)) 
        ]
           
lamLit : Parser TermLit
lamLit =
    succeed curry
        |. lambda
        |= loop [] varLits
        |= Parser.lazy (\() -> termLit)

curry : List String -> TermLit -> TermLit
curry vars body = case vars of
                      [] -> body
                      v::vs -> LamLit v <| curry vs body

parser : Parser TermLit
parser =
    succeed identity
       |. spaces
       |= termLit
       |. end 

-- show
printLit term =
    case term of
        VarLit name -> name
        AppLit m n -> "(" ++ printLit m ++ printLit n ++ ")"
        LamLit x m -> "(\\" ++ x ++"." ++ printLit m ++ ")"

problem2String : Problem -> String
problem2String problem = case problem of
                             Expecting str -> "expectiong " ++ str 
                             ExpectingVariable -> "expecting variable"
                             ExpectingSymbol str -> "expecting symbol " ++ str
                             ExpectingKeyword str -> "expecting keyword" ++ str
                             ExpectingEnd -> "execting end"
                             UnexpectedChar -> "unexpected char"
                             Problem str -> "problem " ++ str
                             other -> "error!"

