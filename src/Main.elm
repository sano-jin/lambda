module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import LambdaParser exposing (..)
import Set as S
import Dict as D
import Lambda exposing (..)

-- main                               
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
                        Ok term ->
                            let (tfv, _, _) = lit2TFV term [] D.empty 0 in
                            let valStr = showTFV tfv in
                            { model | result = valStr , errors = [] }
                        Err err -> { model | result = "", errors = err }
                    
        Change str ->
            { model | inputString = str }

-- View

css path =
    node "link" [rel "stylesheet", href path ] []

view : Model -> Html Msg
view model =
    div [ id "lambda-evaluator" ]
        [ node "link"
              [rel "stylesheet"
              , href "https://fonts.googleapis.com/css2?family=Inconsolata:wght@300&display=swap"
              ] []
        , css "style.css"
        , div [ id "console" ] [ input [ id "expression-reader"
                         , placeholder "input lambda expression \u{23CE}"
                         , value model.inputString, onInput Change ] []
                 , button [ id "expression-submitter"
                          , onClick <| Eval model.inputString ] [ text "parse" ]
                 , div [] [ text (model.result) ]
                 ]
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

