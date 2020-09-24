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
type alias States = { dict : D.Dict String Int, n : Int }
    
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

vm : TermAndFV -> State TermAndFV
vm termAndFV =
    State termAndFV
        <| Tuple.first <| eval (termAndFV, []) { dict = D.singleton (toPostfixNotation termAndFV []) 0
                                               , n = 1 }   
    
evalChildren fun val termPath states =
    let (funChildren, funStates) =
            eval (fun, AppLeftCrumb val::termPath) states in
    let (valChildren, valStates) =
            eval (val, AppRightCrumb fun::termPath) funStates in
    (funChildren ++ valChildren, valStates)
                
eval : TermZipper -> States -> (List (Trans TermAndFV), States)
eval (termAndFV, termPath) states =
        case termAndFV.term of
            AppVal fun val ->
                case fun.term of
                    LamVal var body ->
                        let (evalued, states_) = betaTrans var body val termPath states in
                        let (children, states__) = evalChildren fun val termPath states_ in
                        (BetaTrans evalued::children, states__) 
                    _ -> evalChildren fun val termPath states
            LamVal var body ->
                case body.term of
                    AppVal fun val ->
                        if val.term == VarVal var && not (S.member var fun.fv) then
                            let (evalued, states_) = etaTrans fun termPath states in
                            let (children, states__) = eval (body, LamCrumb var::termPath) states_ in
                            (EtaTrans evalued::children, states__) 
                        else eval (body, LamCrumb var::termPath) states
                    _ -> eval (body, LamCrumb var::termPath) states
            _ -> ([], states)

betaTrans : String -> TermAndFV -> TermAndFV -> List TermCrumb -> States -> (State TermAndFV, States)
betaTrans var body val termPath states =
    let evaluedTerm = substitute var body val
        (evalued, _) = topMostTerm (evaluedTerm, termPath)
        postfix = toPostfixNotation evalued []
    in
        case D.get postfix states.dict of
            Nothing ->
                let (children, states_ ) =
                        eval (evalued, []) { dict = D.insert postfix states.n states.dict
                                           , n = states.n + 1 } in
                (State evalued children, states_)
            Just i -> (Join (states.n - i), { states | n = states.n + 1 })

etaTrans : TermAndFV -> List TermCrumb -> States -> (State TermAndFV, States)
etaTrans fun termPath states =
    let (term, _) = topMostTerm (fun, termPath)
        postfix = toPostfixNotation term []
    in
        case D.get postfix states.dict of
            Nothing ->
                let (children, states_) =
                        eval (term, []) { dict = D.insert postfix states.n states.dict
                                        , n = states.n + 1 } in
                (State term children, states_)
            Just i -> (Join (states.n - i), { states | n = states.n + 1 })
