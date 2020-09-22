module Lambda exposing (..)

import LambdaParser exposing (..)
import Set as S
import Dict as D

type alias TermAndFV = { term : TermVal, fv : S.Set String }
type TermVal = VarVal Int 
             | AppVal TermAndFV TermAndFV
             | LamVal String TermAndFV

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
            ({ term = LamVal var bTFV, fv = S.remove var bTFV.fv}, bCtx)

shift : TermAndFV -> Int -> Int -> TermAndFV
shift termAndFV i c =
    { termAndFV | term = 
          case termAndFV.term of
              VarVal n -> if n < c then VarVal n 
                          else VarVal <| n + i
              AppVal fun val -> AppVal (shift fun i c)  (shift val i c)
              LamVal var body -> LamVal var <| shift body i (c + 1)
    }

substitute : TermAndFV -> TermAndFV -> Int -> TermAndFV
substitute termAndFV1 termAndFV2 m =
    case termAndFV1.term of
        VarVal n -> if n == m then termAndFV2
                    else termAndFV1
        AppVal fun val ->
            let (fun_, val_) =  (substitute fun val m, substitute fun val m) in
            { term = AppVal fun_ val_
            , fv = S.union fun_.fv val_.fv
            }
        LamVal var body -> let (var_, body_) =
                                   if S.member var termAndFV2.fv then
                                       let var__ = newVar var var
                                                   <| S.union termAndFV2.fv body.fv in
                                       (var__
                                       , substitute body {term = VarVal 0
                                                         , fv = S.singleton var__ } 0)
                                   else (var, body)
                               body__ = substitute body_ (shift termAndFV2 1 0) (m + 1)
                           in { term = LamVal var_ body__
                              , fv = S.remove var_ body__.fv
                              }

beta : String -> TermAndFV -> TermAndFV -> TermAndFV
beta var body val =
    let val_ = shift val 1 0
        val__ = substitute body val_ 0
    in
        shift val_ -1 0
                                   
-- convert to postfix notation
toPostfixNotation : TermAndFV -> String
toPostfixNotation tFV =
    case tFV.term of
        VarVal i -> String.fromInt i
        AppVal m n -> toPostfixNotation m ++ toPostfixNotation m ++ "@"
        LamVal _ body -> toPostfixNotation body ++ "\\"

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

showCurriedAbs tFV env =
    case tFV.term of
        VarVal _ -> "." ++ showT tFV env
        AppVal tFV1 tFV2 -> "." ++ showAppFun tFV1 env ++ showAppVal tFV2 env
        LamVal var body -> var ++ showCurriedAbs body (var::env) 

showAppFun tFV env =
    case tFV.term of
        VarVal _ -> showT tFV env
        AppVal tFV1 tFV2 -> showAppFun tFV1 env ++ showAppVal tFV2 env
        LamVal var body ->
            "(\\" ++ var ++ showCurriedAbs body (var::env) ++ ")"

                                    
showAppVal tFV env =
    case tFV.term of
        VarVal _ -> showT tFV env
        AppVal tFV1 tFV2 -> "(" ++ showAppFun tFV1 env ++ showAppVal tFV2 env ++ ")"
        LamVal var body -> showAppFun tFV env
               
newVar : String -> String -> S.Set String -> String
newVar start var fv =
    case S.member var fv of
        False -> var
        True ->
            if start <= "z" && start == var then newVar start "\u{00C0}" fv
            else case var of
                     "z" -> newVar start "A" fv
                     "Z" -> newVar start "a" fv
                     _ ->  newVar start (String.map
                                             (\ch -> Char.fromCode
                                                     <| Char.toCode ch + 1) var) fv                    
