module Zipper exposing (..)

import Lambda exposing (..)
import Set as S

type Trans a = Join Int
             | Beta a (List (Trans a))
             | Alpha a (Trans a)
             | Eta a (List (Trans a))
            
type CrumbTrans a = BetaCrumb a (List (Trans a)) 
                  | AlphaCrumb a
                  | EtaCrumb a (List (Trans a))

type alias TransZipper a = (Trans a, List (CrumbTrans a))

type CrumbTerm = AppLeftCrumb TermAndFV (S.Set String)
               | AppRightCrumb TermAndFV (S.Set String)
               | LamCrumb String (S.Set String)

type alias TermZipper = (TermAndFV, List CrumbTerm)

    
goUp : TransZipper a -> TransZipper a
goUp zipper =
    case zipper of
        (a1, BetaCrumb a2 children::bs) -> (Beta a2 (a1::children), bs)
        (a1, AlphaCrumb a2::bs) -> (Alpha a2 a1, bs)
        (a1, EtaCrumb a2 children::bs) -> (Eta a2 (a1::children), bs)
        top -> top
    
topMost : TransZipper a -> TransZipper a
topMost zipper =
    case zipper of
        (t, []) -> (t, [])
        z -> topMost (goUp z)

goUpTerm : TermZipper -> TermZipper
goUpTerm zipper =
    case zipper of
        (l, AppLeftCrumb r fv::bs) -> ({ term = AppVal l r, fv = fv }, bs)
        (r, AppRightCrumb l fv::bs) -> ({ term = AppVal l r, fv = fv }, bs)
        (body, LamCrumb var fv::bs) -> ({ term = LamVal var body, fv = fv }, bs)
        top -> top
    
topMostTerm : TermZipper -> TermZipper
topMostTerm zipper =
    case zipper of
        (t, []) -> (t, [])
        z -> topMostTerm (goUpTerm z)

evaluator : TransZipper TermZipper -> TransZipper TermZipper
evaluator (termAndFV, path) = (termAndFV, path)

eval : TermZipper -> TransZipper TermAndFV -> TransZipper TermAndFV
eval (termAndFV, termPath) transZipper =
    case termAndFV.term of
        AppVal fun val ->
            case fun.term of
                LamVal var body ->
                    beta var body val termPath transZipper
                _ -> let transZipper_ = eval (fun, AppLeftCrumb val termAndFV.fv::termPath)
                                      transZipper
                     in eval (val, AppLeftCrumb val termAndFV.fv::termPath)
                         transZipper_
        LamVal var body ->
            case body.term of
                AppVal fun val ->
                    if val.term == VarVal 0 && not (S.member var fun.fv) then
                        eta fun termPath transZipper
                    else eval (body, LamCrumb var termAndFV.fv::termPath) transZipper
                _ -> eval (body, LamCrumb var termAndFV.fv::termPath) transZipper
        _ -> transZipper

beta var body val termPath transZipper = transZipper


eta fun termPath transZipper = transZipper
