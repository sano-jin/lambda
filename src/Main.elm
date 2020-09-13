module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import LambdaParser exposing (..)
import Set

-- Main
type TermVal = VarVal Char Int 
             | AppVal TermVal TermVal (Set.Set Char)
             | LamVal Char TermVal (Set.Set Char)


            
main = Browser.sandbox { init = init
                       , update = update
                       , view = view
                       }

         
-- Model

type alias Model =
    { inputString : String
    , result : String
    , errors : List DeadEnd
    }

init : Model
init =
    { inputString = ""
    , result = ""
    , errors = []
    }

-- Update

type Msg
    = Change String
    | Eval String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Eval str -> case run parser str of
                        Ok term -> { model | result = evalAndPrint term , errors = [] }
                        Err err -> { model | result = "", errors = err }
                    
        Change str ->
            { model | inputString = str }

-- View

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to reverse", value model.inputString, onInput Change ] []
        , button [ onClick <| Eval model.inputString ] [ text "parse" ]
        , div [] [ text (model.result) ]
        , div [] <| List.map
            (\err ->
                 div [] [ text <| problem2String err.problem
                        , div [] [ text <|
                                       "at row: " ++ String.fromInt err.row
                                       ++ ", col: " ++ String.fromInt err.col ]
                        ]
            ) model.errors
        ]                             
-- functions
{--
freeVariable term = case term of
                        VarLit name _ -> Set.singleton name
                        AppLit m n -> Set.union (freeVariable m) (freeVariable n)
                        LamLit x m -> Set.remove x (freeVariable m)

     --}

evalAndPrint term = case term of
                        VarLit name -> String.fromChar name
                        AppLit m n -> "(" ++ evalAndPrint m ++ evalAndPrint n ++ ")"
                        LamLit x m -> "(\\" ++ String.fromChar x ++"." ++ evalAndPrint m ++ ")"

-- parse


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
