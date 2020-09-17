module Zipper exposing (..)

import Lambda exposing (..)
import Set as S

type Trans a = Join Int
             | Beta a (List (Trans a))
             | Alpha a (Trans a)
             | Eta a (Trans a)
            
type CrumbTrans a = BetaCrumb a (List (Trans a))
                  | AlphaCrumb a
                  | EtaCrumb a

type alias TransZipper a = (Trans a, List (CrumbTrans a))

type CrumbTerm = AppLeftCrumb TermAndFV
               | AppRightCrumb TermAndFV
               | LamCrumb String

type alias TermZipper = (TermAndFV, List CrumbTerm)

    
goUp : TransZipper a -> TransZipper a
goUp zipper =
    case zipper of
        (a1, BetaCrumb a2 children::bs) -> (Beta a2 (a1::children), bs)
        (a1, AlphaCrumb a2::bs) -> (Alpha a2 a1, bs)
        (a1, EtaCrumb a2::bs) -> (Eta a2 a1, bs)
        top -> top
    
topMost : TransZipper a -> TransZipper a
topMost zipper =
    case zipper of
        (t, []) -> (t, [])
        z -> topMost (goUp z)

evaluator : TransZipper TermZipper -> TransZipper TermZipper
evaluator (termAndFV, path) = (termAndFV, path)


eval : TransZipper TermZipper -> TransZipper TermZipper
eval ((termAndFV, termPath), transPath) =
    case termAndFV.term of
        AppVal fun val ->
            case fun.term of
                LamVal var body ->
                    beta var body val termPath transPath
                _ -> let (_, transPath_) = eval ((fun
                                                 , LamCrumb var::AppLeftCrumb val::termPath)
                                                , transPath)
                     in 
                         eval ((val
                               , LamCrumb var::AppLeftCrumb val::termPath)
                              , transPath)
        LamVal var body ->
            case body.term of
                AppVal fun val ->
                    if val.term == VarVal 0 && not <| S.member var fun.fv then
                        eta fun termPath transPath
                _ -> let ((termAndFV_, termPath_), transPath_)
                             = eval ((body
                                     , AppCrumb val::AppLeftCrumb val::termPath)
                                    , transPath)
                     in ((LamVal var termAndFV_, termPath_), transPath_)
        _ -> let ((termAndFV, termPath), transPath)
                
