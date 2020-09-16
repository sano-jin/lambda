module Lambda exposing (..)

import LambdaParser exposing (..)
import Set as S
import Dict as D

type alias TermAndFV = { term : TermVal, fv : S.Set String }
type TermVal = VarVal Int 
             | AppVal TermAndFV TermAndFV
             | LamVal String TermAndFV
             | Error

getIndexOf : a -> List a -> Result Int Int
getIndexOf val list =
    let getIndexOfHelp l i = 
            case l of
                [] -> Err i
                h::t -> if h == val then Ok i
                        else getIndexOfHelp t (i + 1)
    in getIndexOfHelp list 0
      
               
-- l2TC termList env context
-- ==> (termAndContext, context)
lit2TFV : TermLit -> List String -> List String 
       -> (TermAndFV, List String)
lit2TFV termLit env ctx =
    case termLit of
        VarLit x ->
            let fv_ = S.singleton x in
            case getIndexOf x env of
                Ok i -> ({ term = VarVal i, fv = fv_}, ctx)
                Err cutoff ->
                    case getIndexOf x ctx of
                        Ok i -> ({ term = VarVal (i + cutoff), fv = fv_}, ctx)
                        Err n -> ({ term = VarVal (n + cutoff)
                                  , fv = fv_
                                  }, ctx ++ [x]
                                 )
        AppLit t1 t2 ->
            let (tFV1, ctx1) = lit2TFV t1 env ctx in
            let (tFV2, ctx2) = lit2TFV t2 env ctx1 in
            ({ term = AppVal tFV1 tFV2, fv = S.union tFV1.fv tFV2.fv}, ctx2)
        LamLit var body ->
            let (bTFV, bCtx) = lit2TFV body (env ++ [var]) ctx in
            ({ term = LamVal var bTFV, fv = bTFV.fv}, bCtx)

-- show

nth : Int -> List a -> Maybe a
nth i list = case list of
                 [] -> Nothing
                 h::t -> if 0 < i then nth (i - 1) t
                         else Just h
    
showT : TermAndFV -> List String -> String
showT tFV env =
    case tFV.term of
        VarVal i ->
            case nth i env of
                Just var -> var
                Nothing -> " Error " -- never reaches here
        AppVal tFV1 tFV2 -> showAppFun tFV1 env ++ showAppVal tFV2 env
        LamVal var body -> "\\" ++ var ++ showCurriedAbs body (var::env) 
        Error -> " Error "

showCurriedAbs tFV env =
    case tFV.term of
        VarVal _ -> "." ++ showT tFV env
        AppVal tFV1 tFV2 -> "." ++ showAppFun tFV1 env ++ showAppVal tFV2 env
        LamVal var body -> var ++ showCurriedAbs body (var::env) 
        Error -> " Error "

showAppFun tFV env =
    case tFV.term of
        VarVal _ -> showT tFV env
        AppVal tFV1 tFV2 -> showAppFun tFV1 env ++ showAppVal tFV2 env
        LamVal var body ->
            "(\\" ++ var ++ showCurriedAbs body (var::env) ++ ")"
        Error -> " error "

                                    
showAppVal tFV env =
    case tFV.term of
        VarVal _ -> showT tFV env
        AppVal tFV1 tFV2 -> "(" ++ showAppFun tFV1 env ++ showAppVal tFV2 env ++ ")"
        LamVal var body -> showT tFV env
        Error -> " error "
               

newVarFromCh : Char -> Char -> S.Set Char -> Char
newVarFromCh start ch fv =
    case S.member ch fv of
        False -> ch
        True -> 
            if start == ch then newVarFromCh start (Char.fromCode <| Char.toCode ch + 1) fv
            else if ch == 'z' then newVarFromCh start 'A' fv
                 else if ch == 'Z' then newVarFromCh start 'a' fv
                 else newVarFromCh start (Char.fromCode <| Char.toCode ch + 1) fv
    
