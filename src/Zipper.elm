module Zipper exposing (..)

type Tree a = Empty
            | Node a (List (Tree a)) 
            
type Crumb a = Crumb a (List (Tree a)) (List (Tree a)) 

type alias Zipper a = (Tree a, List (Crumb a))


    
    

    
