module Zipper exposing (..)

import Lambda exposing (..)
import Set as S

type Tree a = Join Int
            | Beta a (List (Tree a))
            | Alpha a (Tree a)
            
type Crumb a = BetaCrumb a (List (Tree a))
             | AlphaCrumb a

type alias Zipper a = (Tree a, List (Crumb a))

goUp : Zipper a -> Zipper a
goUp zipper =
    case zipper of
        (s1, BetaCrumb s2 children::bs) -> (Beta s2 (s1::children), bs)
        (s1, AlphaCrumb s2::bs) -> (Alpha s2 s1, bs)
        top -> top
    
topMost : Zipper a -> Zipper a
topMost zipper =
    case zipper of
        (t, []) -> (t, [])
        z -> topMost (goUp z)

evaluator : Zipper TermAndFV -> Zipper TermAndFV
evaluator (termAndFV, path) = (termAndFV, path)


eval : TermAndFV -> S.Set TermAndFV
eval (termAndFV, path) =
    case termAndFV.term of
        AppVal fun val ->
            case fun.term of
                LamVal var body ->
                    

    
