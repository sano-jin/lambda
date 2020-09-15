module Lambda exposing (..)

import LambdaParser exposing (..)
import Set as S
import Dict as D

type Tree a = Empty
            | Node a (Tree a) (Tree a)

type Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
type alias Breadcrumbs a = List (Crumb a)

goLeft : Zipper a -> Zipper a 
goLeft node =
    case node of
        (Node x l r, bs) -> (l, LeftCrumb x r::bs)
        empty -> empty

goRight : Zipper a -> Zipper a
goRight node =
    case node of
        (Node x l r, bs) -> (r, RightCrumb x l::bs)
        empty -> empty

goUp : Zipper a -> Zipper a 
goUp node =
    case node of
        (t, LeftCrumb x r::bs) -> (Node x t r, bs)
        (t, RightCrumb x l::bs) -> (Node x l t, bs)
        top -> top

type alias Zipper a = (Tree a, Breadcrumbs a)

modify : (a -> a) -> Zipper a -> Zipper a
modify f node =
    case node of
        (Node x l r, bs) -> (Node (f x) l r, bs)
        empty -> empty

break : (a -> Bool) -> List a -> (List a, List a)
break f list = case list of
                   [] -> ([], [])
                   (h::t) as l ->
                       if f h then ([], l)
                       else
                           let (pred, suc) = break f t in
                           (h::pred, suc)



                           
                                             
type alias TermAndFV = { term : TermVal, fv : S.Set Char }
type TermVal = VarVal Char Int 
             | AppVal TermAndFV TermAndFV
             | LamVal Char TermAndFV
             | Error

getIndexOf : a -> List a -> Result Int Int
getIndexOf val list =
    let getIndexOfHelp l i = 
            case l of
                [] -> Err i
                h::t -> if h == val then Ok i
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
                Ok i -> ({ term = VarVal x i, fv = S.empty}, ctx, n)
                Err cutoff ->
                    case D.get x ctx of
                        Just i -> ({ term = VarVal x (i + cutoff), fv = S.singleton x}, ctx, n)
                        Nothing -> ({ term = VarVal x (n + cutoff)
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

-- show
showTFV : TermAndFV -> String
showTFV tFV = let fvs = "{" ++ String.join ", "
                        (List.map String.fromChar <| S.toList tFV.fv) ++ "}"
              in showT tFV ++ " " ++ fvs

showT : TermAndFV -> String
showT tFV = case tFV.term of
                VarVal x i -> String.fromChar x -- ++ String.fromInt i 
                AppVal tFV1 tFV2 -> showAppFun tFV1 ++ "" ++ showAppVal tFV2
                LamVal var body -> "\\" ++ String.fromChar var ++ showCurriedAbs body
                Error -> " error "

showCurriedAbs tFV = case tFV.term of
                VarVal x i -> "." ++ String.fromChar x -- ++ String.fromInt i 
                AppVal tFV1 tFV2 -> "." ++ showAppFun tFV1 ++ "" ++ showAppVal tFV2
                LamVal var body -> String.fromChar var ++ showCurriedAbs body
                Error -> " error "

showAppFun tFV = case tFV.term of
                     VarVal x i -> String.fromChar x -- ++ String.fromInt i 
                     AppVal tFV1 tFV2 -> showAppFun tFV1 ++ "" ++ showAppVal tFV2
                     LamVal var body ->
                         "(\\" ++ String.fromChar var ++ showCurriedAbs body ++ ")"
                     Error -> " error "

                                    
showAppVal tFV = case tFV.term of
                     VarVal x i -> String.fromChar x -- ++ String.fromInt i 
                     AppVal tFV1 tFV2 -> "(" ++ showT tFV1 ++ "" ++ showT tFV2 ++ ")"
                     LamVal var body -> showT tFV
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
    
