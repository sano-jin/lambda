module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run, DeadEnd, Problem (..))
import LambdaParser exposing (..)
import Set as S
import Dict as D

-- Main
type alias TermAndFV = { term : TermVal, fv : S.Set Char }
type TermVal = VarVal Char Int 
             | AppVal TermAndFV TermAndFV
             | LamVal Char TermAndFV
             | Error

getIndexOf : a -> List a -> Maybe Int
getIndexOf val list =
    let getIndexOfHelp l i = 
            case l of
                [] -> Nothing
                h::t -> if h == val then Just i
                        else getIndexOfHelp t (i + 1)
    in getIndexOfHelp list 0
      
               
-- l2TC termList env context numberOfFreeVariable
-- ==> (termAndContext, context, numberOfFreeVariables)
lit2TFV : TermLit -> List Char -> D.Dict Char Int -> Int
       -> (TermAndFV, D.Dict Char Int, Int)
lit2TFV termLit env ctx n =
    case termLit of
        VarLit x ->
            case getIndexOf x env of
                Just i -> ({ term = VarVal x i, fv = S.empty}, ctx, n)
                Nothing ->
                    case D.get x ctx of
                        Just i -> ({ term = VarVal x i, fv = S.singleton x}, ctx, n)
                        Nothing -> ({ term = VarVal x n
                                    , fv = S.singleton x
                                    }, D.insert x n ctx
                                   , n + 1)
        AppLit t1 t2 ->
            let (tFV1, ctx1, n1) = lit2TFV t1 env ctx n in
            let (tFV2, ctx2, n2) = lit2TFV t2 env ctx1 n1 in
            ({ term = AppVal tFV1 tFV2, fv = S.union tFV1.fv tFV2.fv}, ctx2, n2)
        LamLit var body ->
            let (ctx_, n_) = if D.member var ctx then (D.remove var ctx, n - 1) else (ctx, n) in
            let (bTFV, bCtx, bN) = lit2TFV body (var::env) ctx_ n_ in
            ({ term = LamVal var bTFV, fv = bTFV.fv}, bCtx, bN)
            


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
                        Ok term -> { model | result = printLit term , errors = [] }
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

