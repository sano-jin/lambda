module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, node, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import LambdaParser exposing (..)
import Set as S
import Dict as D
import Lambda exposing (..)
import VM exposing (..)

-- main                               
main = Browser.sandbox { init = init
                       , update = update
                       , view = view
                       }

         
-- Model

type alias Model =
    { inputString : String
    , result : String
    , states : Maybe (State TermAndFV)
    , ctx : List String
    , errors : List DeadEnd
    }

init : Model
init =
    { inputString = ""
    , result = ""
    , states = Nothing
    , ctx = []
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
                            let (tfv, ctx) = lit2TFV term [] [] in
                            let valStr = showT tfv ++ " [" ++ String.join "," ctx ++ "]" in
                            { model | result = valStr , errors = [], ctx = ctx, states = Just <| vm tfv }
                        Err err -> { model | result = "", errors = err, states = Nothing }
                    
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
        , div [ id "console" ]
            [ input [ id "expression-reader"
                    , placeholder "input lambda expression \u{23CE}"
                    , value model.inputString, onInput Change ] []
            , button [ id "expression-submitter"
                     , onClick <| Eval model.inputString ] [ text "parse" ]
            , div [] [ text (model.result) ]
            ]
        , div [ id "states" ] [
               case model.states of
                   Nothing -> text "Nothing"
                   Just states -> ul [] [ view_of_states states "root" ]
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

view_of_states : State TermAndFV -> String -> Html Msg
view_of_states state transType =
    case state of
        Join i -> li [ style "height" <| String.fromInt (i * 20)
                      , class "join"
                      , class transType ]
                  [ text <| transType ++ " join " ++ String.fromInt i ]
        State term children ->
            li [ class transType ]
                [ div [] [ text transType ]
                , div [ class "term" ] [ text <| showT term ]
                , ul [ class "children" ] <|
                    List.map (\trans -> case trans of
                                            BetaTrans s -> view_of_states s "beta"
                                            EtaTrans s -> view_of_states s "eta"
                             ) children
                ]
    
