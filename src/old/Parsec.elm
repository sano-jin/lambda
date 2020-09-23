module Parsec exposing (..)

import Set
import Char exposing (..)

type Error = UnexpectedCharactor Char
           | ExpectedCharactor Char Char
           | UnexpectedEnd
           | ComponentError

type alias Lazy a = (() -> a)

lazy : a -> Lazy a
lazy a = (\() -> a)

force : Lazy a -> a
force a = a ()

          
type alias Parser a = Lazy ((List Char, Int) -> (Result Error a, (List Char, Int))) 

or : List (Parser a) -> Parser (List a)
or parsers =
    lazy (\input ->
              case parsers of
                  [] -> (Err ComponentError, input)
                  [parser] -> force (apply (\x -> [x]) parser) input
                  p::ps    ->
                      case force p input of
                          (Ok val, rest)  -> force(apply (\vals -> val::vals) (or ps)) rest
                          (Err err, rest) -> (Err err, rest)
         )
    
and : Parser a -> Parser b -> Parser (a, b)
and parser1 parser2 =
    lazy (\input ->
              case force parser1 input of
                  (Err err1, rest1) -> (Err err1, rest1)
                  (Ok val1, rest1) -> force (apply (\val2 -> (val1, val2)) parser2) rest1
         )
        
many : Parser a -> Parser (List a)
many parser =
    lazy (\input ->
          case force parser input of
              (Err _, _) -> (Ok [], input)
              (Ok val1, rest1) ->
                  force(apply (\val2 -> val1::val2) (many parser)) rest1
         )
        
many1 : Parser a -> Parser (List a)
many1 parser = apply (\(v1, v2) -> v1::v2) <| and parser (many parser)
               
apply : (a -> b) -> Parser a -> Parser b
apply f parser =
    lazy (\input ->
              case force parser input of
                  (Ok val, rest)  -> (Ok <| f val, rest)
                  (Err err, rest) -> (Err err, rest)
         )
    
char : Char -> Parser Char
char c =
    lazy (\(charList, i) ->
              case charList of
                  [] -> (Err UnexpectedEnd, (charList, i))
                  x::xs -> if x == c then (Ok x, (xs, i + 1))
                           else (Err <| ExpectedCharactor x c, (xs, i))
         )
        
charF : (Char -> Bool) -> Parser Char
charF f =
    lazy(\(charList, i) ->
             case charList of
                 [] -> (Err UnexpectedEnd, (charList, i))
                 x::xs -> if f x then (Ok x, (xs, i + 1))
                          else (Err <| UnexpectedCharactor x, (xs, i))
        )
        
isSpace : Char -> Bool
isSpace ch = case toCode ch of
                 32 -> True
                 160 -> True
                 _ -> False

lit : Char -> Parser Char
lit = lexeme << char
      
letter : Parser Char
letter = charF (\x -> isUpper x || isLower x)

space : Parser Char
space = charF isSpace

spaces : Parser (List Char)
spaces = many space

spaces1 : Parser (List Char)
spaces1 = many1 space
        
lexeme : Parser a -> Parser a
lexeme parser = apply (\(val, _) -> val) <| and parser <| many space

paren : Parser a -> Parser a
paren parser = apply
               (\((_, val), _) -> val)
               (and
                (and
                 (lexeme (char '('))
                 (lexeme parser)
                ) (lexeme (char ')'))
               )
                    
