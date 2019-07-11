{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
module RBNF.Symbols where

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L
import Control.Monad.State
import Control.Arrow
import Control.Lens (over, view, Lens')

type Set a = S.Set a
type Map a b = M.Map a b

data MatchCond = forall a. (Show a, Eq a, Ord a) => MatchCond {predicate :: String, value :: a}
instance Eq MatchCond
instance Ord MatchCond
instance Show MatchCond where
    show MatchCond {predicate=pred, value=val} = pred ++ " = " ++ show val

showConds :: (Foldable t, Show a) => t a -> String
showConds = foldl f []
            where f [] a = show a
                  f xs a = show a ++ "&&" ++ xs

newtype Case = Case (Set MatchCond)
    deriving (Eq, Ord, Show)


data MiniLang
    = MTerm String
    | MApp String [MiniLang]
    deriving (Eq, Ord)

instance Show MiniLang where
    show = \case
        MTerm s -> s
        MApp f args ->
            f ++ "("
              ++  L.intercalate ", " (map show args)
              ++ ")"

-- Parsing symbols
-- Also can be regarded as IRs with
-- stack-based VM instruction semantics.
data P
    = PTerm Case
    -- PNonTerm symbol is_grammar_node
    | PNonTerm String
    -- advanced:
    -- PPack TopN_of_stack Reduce_func
    | PReduce Int MiniLang

    | PPred MiniLang

    | PBind String

    | PModif MiniLang
    deriving (Eq, Ord)

type PRule = [P]
type PProd = (String, PRule) -- productions
instance Show P where
    show = \case
        PTerm (Case set)  -> "<" ++ showConds set ++ ">"
        PNonTerm s     -> s
        --
        PPred    apply -> "?" ++ show apply
        PReduce  n app -> "reduce<" ++ show n ++ ", " ++ show app ++ ">"
        PBind    s     -> "=: " ++ s
        PModif   modif -> "!" ++ show modif

-- Combinatorial
data C
    = CTerm    [MatchCond]
    | CNonTerm String
    | CSeq     [C]
    | CAlt     [C]
    -- advanced:
    | CBind    String C
    | CPred    MiniLang
    | CModif   MiniLang -- modify current context
    deriving (Eq, Ord)

type CRule = C
data CProd = CProd String C (Maybe MiniLang)
instance Show C where
    show = \case
        CTerm set   -> "<" ++ showConds set ++ ">"
        CNonTerm s  -> s
        CSeq [c]    -> show c
        CSeq cs     -> "(" ++ (L.intercalate " " $ map show cs) ++ ")"
        CAlt [c]    -> show c
        CAlt cs     -> "(" ++ (L.intercalate "|" $ map show cs) ++ ")"
        --
        CBind name c -> name ++ "=" ++ show c
        CPred apply  -> "?" ++ show apply
        CModif modif -> "!" ++ show modif