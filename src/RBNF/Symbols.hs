{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
module RBNF.Symbols where

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L
import Control.Monad.State
import Control.Arrow
import Control.Lens (over, view, Lens')

type Set a = S.Set a
type Map a b = M.Map a b

data Case = forall a. (Eq a, Ord a, Show a) => Case {predicate :: String, value :: a}

instance Eq Case
instance Ord Case
instance Show Case where
    show Case {predicate, value} = "<" ++ predicate ++ "="  ++ show value ++ ">"



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
    | PPack   Int
    | PReduce MiniLang Int

    | PPred MiniLang

    | PBind String

    | PModif MiniLang

    | PMkSExp String
    deriving (Eq, Ord)

type PRule = [P]
type PProd = (String, PRule) -- productions
instance Show P where
    show = \case
        PTerm c        -> "<" ++ show c ++ ">"
        PNonTerm s     -> s
        --
        PPack    n     -> "pack<" ++ show n ++ ">"
        PPred    apply -> "?" ++ show apply
        PReduce  app n -> "reduce<" ++ show n ++ ", " ++ show app ++ ">"
        PBind    s     -> "=: " ++ s
        PModif   modif -> "!" ++ show modif
        PMkSExp  tag   -> "s-exp<" ++ tag ++ ">"

-- Combinatorial
data C
    = CTerm    Case
    | CNonTerm String
    | CSeq     [C]
    | CAlt     [C]
    -- advanced:
    | CBind    String C
    | CPred    MiniLang
    | CModif   MiniLang -- modify current context
    deriving (Eq, Ord)

type CRule = C
type CProd = (String, C, (Maybe MiniLang))
instance Show C where
    show = \case
        CTerm c     -> "<" ++ show c ++ ">"
        CNonTerm s  -> s
        CSeq [c]    -> show c
        CSeq cs     -> "(" ++ unwords (map show cs) ++ ")"
        CAlt [c]    -> show c
        CAlt cs     -> "(" ++ L.intercalate "|" (map show cs) ++ ")"
        --
        CBind name c -> name ++ "=" ++ show c
        CPred apply  -> "?" ++ show apply
        CModif modif -> "!" ++ show modif