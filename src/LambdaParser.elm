module LambdaParser exposing
    ( TermLit (..)
    , parser
    )       

import Parser exposing (..)
import Char

type TermLit = VarLit Char
             | AppLit TermLit TermLit
             | LamLit Char TermLit

-- Lexer

lexeme : Parser a -> Parser a
lexeme p = p |. spaces 

lambda : Parser ()
lambda = lexeme <| oneOf [ symbol "\\"
                         , symbol "\u{03BB}"  -- unicode lambda
                         , symbol "\u{00A5}"  -- yen mark
                         ]

isSpace : Char -> Bool
isSpace c = case c of
                ' ' -> True
                '\n' -> True
                '\r' -> True
                _ -> False
         
varLit : Parser TermLit
varLit = chompIf Char.isAlpha
       |> getChompedString
       |> lexeme
       |> Parser.map (\str -> case String.toList str of
                                  [c] -> VarLit c
                                  _ -> VarLit 'x' -- never reaches here
                     )

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
          , varLit
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
    
varLits : Parser (List Char)
varLits = chompWhile (\c -> Char.isAlpha c || isSpace c) 
        |> getChompedString
        |> andThen
           (\x -> let vars = List.filter (not << isSpace) <| String.toList x
                  in case List.length vars of
                         0 -> problem "0 variable"
                         _ -> succeed vars
          )
           
lamLit : Parser TermLit
lamLit =
    succeed curry
        |. lambda
        |= varLits
        |. lexeme (symbol ".")
        |= Parser.lazy (\() -> termLit)

curry : List Char -> TermLit -> TermLit
curry vars body = case vars of
                      [] -> body
                      v::vs -> LamLit v <| curry vs body

parser : Parser TermLit
parser =
    succeed identity
       |. spaces
       |= termLit
       |. end 

