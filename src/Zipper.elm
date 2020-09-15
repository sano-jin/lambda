module Zipper exposing (..)

type Tree a = Join Int
            | Beta a (List (Tree a))
            | Alpha a (Tree a)
            
type Crumb a = BetaCrumb a (List (Tree a))
             | AlphaCrumb a

type alias Zipper a = (Tree a, List (Crumb a))

goUp : Zipper a -> Zipper a
goUp zipper =
    case zipper of
        (s1, BetaCrumb s2 children::bs) -> (Beta s2 (children ++ [s1]), bs)
        (s1, AlphaCrumb s2::bs) -> (Alpha s2 s1, bs)
        top -> top
    
topMost : Zipper a -> Zipper a
topMost zipper =
    case zipper of
        (t, []) -> (t, [])
        z -> topMost (goUp z)


    
