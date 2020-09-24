module VM exposing (..)

import Dict as D
import Lambda exposing (..)
import Set as S

type Trans a = BetaTrans (State a)
             | EtaTrans (State a)
    
    
type State a = State a (List (Trans a))
             | Join Int

type TermCrumb = AppLeftCrumb TermAndFV
               | AppRightCrumb TermAndFV
               | LamCrumb String
                 
type alias TermZipper = (TermAndFV, List TermCrumb)


goUpTerm : TermZipper -> TermZipper
goUpTerm zipper =
    case zipper of
        (l, AppLeftCrumb r::bs) -> ({ term = AppVal l r, fv = S.union l.fv r.fv }, bs)
        (r, AppRightCrumb l::bs) -> ({ term = AppVal l r, fv = S.union l.fv r.fv }, bs)
        (body, LamCrumb var::bs) ->
            ({ term = LamVal var body, fv = S.remove var body.fv }, bs)
        top -> top
    
topMostTerm : TermZipper -> TermZipper
topMostTerm zipper =
    case zipper of
        (t, []) -> (t, [])
        z -> topMostTerm (goUpTerm z)

evaluator : TermZipper -> D.Dict String Int -> Int -> (State TermAndFV, D.Dict String Int, Int)
evaluator (termAndFV, termPath) statesDict i = (Join 0, statesDict, i)

vm : TermAndFV -> State TermAndFV
vm termAndFV =
    let (children, _, _) = eval (termAndFV, []) (D.singleton (toPostfixNotation termAndFV []) 0) 0 in
    State termAndFV children
    
evalChildren fun val termPath statesDict stateIndex =
    let (funChildren, funStatesDict, funStateIndex) =
            eval (fun, AppLeftCrumb val::termPath) statesDict stateIndex in
    let (valChildren, valStatesDict, valStateIndex) =
            eval (val, AppRightCrumb fun::termPath) funStatesDict funStateIndex in
    (funChildren ++ valChildren, valStatesDict, valStateIndex)
                
eval : TermZipper -> D.Dict String Int -> Int -> (List (Trans TermAndFV), D.Dict String Int, Int)
eval (termAndFV, termPath) statesDict stateIndex =
        case termAndFV.term of
            AppVal fun val ->
                case fun.term of
                    LamVal var body ->
                        let (evalued, statesDict_, stateIndex_) = betaTrans var body val termPath statesDict stateIndex in
                        let (children, statesDict__, stateIndex__) = evalChildren fun val termPath statesDict_ stateIndex_ in
                        (BetaTrans evalued::children, statesDict__, stateIndex__) 
                    _ -> evalChildren fun val termPath statesDict stateIndex
            LamVal var body ->
                case body.term of
                    AppVal fun val ->
                        if val.term == VarVal var && not (S.member var fun.fv) then
                            let (evalued, statesDict_, stateIndex_) = etaTrans fun termPath statesDict stateIndex in
                            let (children, statesDict__, stateIndex__) = eval (body, LamCrumb var::termPath) statesDict_ stateIndex_ in
                            (EtaTrans evalued::children, statesDict__, stateIndex__) 
                        else eval (body, LamCrumb var::termPath) statesDict stateIndex
                    _ -> eval (body, LamCrumb var::termPath) statesDict stateIndex
            _ -> ([], statesDict, stateIndex)

betaTrans : String -> TermAndFV -> TermAndFV -> List TermCrumb -> D.Dict String Int -> Int -> (State TermAndFV, D.Dict String Int, Int)
betaTrans var body val termPath statesDict stateIndex =
    let evaluedTerm = beta var body val
        (evalued, _) = topMostTerm (evaluedTerm, termPath)
        postfix = toPostfixNotation evalued []
    in
        case D.get postfix statesDict of
            Nothing ->
                let (children, statesDict_, stateIndex_) =
                        eval (evalued, []) (D.insert postfix stateIndex statesDict) (stateIndex + 1) in
                (State evalued children, statesDict_, stateIndex_)
            Just i -> (Join (stateIndex - i), statesDict, stateIndex + 1)

etaTrans : TermAndFV -> List TermCrumb -> D.Dict String Int -> Int -> (State TermAndFV, D.Dict String Int, Int)
etaTrans fun termPath statesDict stateIndex =
    let (term, _) = topMostTerm (fun, termPath)
        postfix = toPostfixNotation term []
    in
        case D.get postfix statesDict of
            Nothing ->
                let (children, statesDict_, stateIndex_) =
                        eval (term, []) (D.insert postfix stateIndex statesDict) (stateIndex + 1) in
                (State term children, statesDict_, stateIndex_)
            Just i -> (Join (stateIndex - i), statesDict, stateIndex + 1)
