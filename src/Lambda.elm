module Lambda exposing (..)

import LambdaParser exposing (..)
import Set as S
import Dict as D

type alias TermAndFV = { term : TermVal, fv : S.Set String }
type TermVal = VarVal String 
             | AppVal TermAndFV TermAndFV
             | LamVal String TermAndFV

lit2TFV : TermLit -> List String -> TermAndFV
lit2TFV termLit env =
    case termLit of
        VarLit x -> { term = VarVal x, fv = S.singleton x}
        AppLit t1 t2 ->
            let tFV1 = lit2TFV t1 env
                tFV2 = lit2TFV t2 env in
            { term = AppVal tFV1 tFV2, fv = S.union tFV1.fv tFV2.fv}
        LamLit var body ->
            let bTFV = lit2TFV body (var::env) in
            { term = LamVal var bTFV, fv = S.remove var bTFV.fv}

substitute : String -> TermAndFV -> TermAndFV -> TermAndFV
substitute var termAndFV1 termAndFV2 =
    case termAndFV1.term of
        VarVal x -> if x == var then termAndFV2
                    else termAndFV1
        AppVal fun val ->
            let (fun_, val_) =  (substitute var fun termAndFV2, substitute var val termAndFV2) in
            { term = AppVal fun_ val_
            , fv = S.union fun_.fv val_.fv
            }
        LamVal x body ->
            if x == var then termAndFV1
            else let (x_, body_) =
                         if S.member x termAndFV2.fv then
                             let z = newVar x
                                         <| S.union termAndFV2.fv body.fv in
                             (z, substitute x body {term = VarVal z, fv = S.singleton z })
                         else (x, body)
                     body__ = substitute var body_ termAndFV2
                 in { term = LamVal x_ body__
                    , fv = S.remove x_ body__.fv
                    }

-- convert to postfix notation
getIndex : a -> List a -> Maybe Int
getIndex x list =
    let getIndexHelp l i =
            case l of
                [] -> Nothing
                h::t -> if h == x then Just i
                        else getIndexHelp t (i + 1)
    in getIndexHelp list 0
    
toPostfixNotation : TermAndFV -> List String -> String
toPostfixNotation tFV env =
    case tFV.term of
        VarVal x -> case getIndex x env of
                        Nothing -> x
                        Just i -> String.fromInt i
        AppVal m n -> toPostfixNotation m env ++ toPostfixNotation n env ++ "@"
        LamVal x body -> toPostfixNotation body (x::env) ++ "\\"

-- show
showT : TermAndFV -> String
showT tFV =
    case tFV.term of
        VarVal x -> x
        AppVal tFV1 tFV2 -> showAppFun tFV1 ++ showAppVal tFV2
        LamVal var body -> "\\" ++ var ++ showCurriedAbs body 

showCurriedAbs tFV =
    case tFV.term of
        LamVal var body -> var ++ showCurriedAbs body
        _ -> "." ++ showT tFV

showAppFun tFV =
    case tFV.term of
        LamVal var body ->
            "(" ++ showT tFV ++ ")"
        _ -> showT tFV

showAppVal tFV =
    case tFV.term of
        VarVal _ -> showT tFV
        AppVal tFV1 tFV2 -> "(" ++ showT tFV ++ ")"
        LamVal var body -> showAppFun tFV
               
newVar : String -> S.Set String -> String
newVar var fv =
    case S.member var fv of
        False -> var
        True ->
            case var of
                "z" -> newVar "A" fv
                "Z" -> newVar "a" fv
                _ ->  newVar (String.map
                                  (Char.fromCode << (+) 1 << Char.toCode) var) fv
                      
                      
