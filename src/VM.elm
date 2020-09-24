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
        <| Tuple.first
            <| eval (termAndFV, []) { dict = D.singleton (toPostfixNotation termAndFV []) 0
                                    , n = 1 }   

update : (s -> (a, s)) -> (s -> (b, s)) -> (a -> b -> c) -> s -> (c, s)
update f g h s =
    let (a, s_) = f s
        (b, s__) = g s_ in
    (h a b, s__)    

evalChildren : TermAndFV -> TermAndFV -> List TermCrumb -> States -> (List (Trans TermAndFV), States)
evalChildren fun val termPath =
    update (eval (fun, AppLeftCrumb val::termPath)) (eval (val, AppRightCrumb fun::termPath)) (++)
        
eval : TermZipper -> States -> (List (Trans TermAndFV), States)
eval (termAndFV, termPath) =
        case termAndFV.term of
            AppVal fun val ->
                case fun.term of
                    LamVal var body ->
                        update (betaTrans var body val termPath) (evalChildren fun val termPath)
                            (\e es -> BetaTrans e::es)
                    _ -> evalChildren fun val termPath
            LamVal var body ->
                case body.term of
                    AppVal fun val ->
                        if val.term == VarVal var && not (S.member var fun.fv) then
                            update (etaTrans fun termPath) (eval (body, LamCrumb var::termPath))
                                (\e es -> EtaTrans e::es)
                        else eval (body, LamCrumb var::termPath)
                    _ -> eval (body, LamCrumb var::termPath)
            _ -> Tuple.pair []

betaTrans : String -> TermAndFV -> TermAndFV -> List TermCrumb -> States -> (State TermAndFV, States)
betaTrans var body val termPath states =
    let evaluedTerm = substitute var body val
        (evalued, _) = topMostTerm (evaluedTerm, termPath)
        postfix = toPostfixNotation evalued []
    in
        case D.get postfix states.dict of
            Nothing ->
                Tuple.mapFirst (State evalued)
                    <| eval (evalued, []) { dict = D.insert postfix states.n states.dict
                                           , n = states.n + 1 }
            Just i -> (Join (states.n - i), { states | n = states.n + 1 })

etaTrans : TermAndFV -> List TermCrumb -> States -> (State TermAndFV, States)
etaTrans fun termPath states =
    let (term, _) = topMostTerm (fun, termPath)
        postfix = toPostfixNotation term []
    in
        case D.get postfix states.dict of
            Nothing ->
                Tuple.mapFirst (State term)
                        <| eval (term, []) { dict = D.insert postfix states.n states.dict
                                           , n = states.n + 1 }
            Just i -> (Join (states.n - i), { states | n = states.n + 1 })
