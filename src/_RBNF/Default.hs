{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module RBNF.Default where
import RBNF.Grammar
import RBNF.ConfSet

import qualified Data.Map  as M
import qualified Data.List as L
import Control.Monad.State
import Control.Lens (view, over)
import Data.Maybe (fromJust)

data SimplePGrammar
    = SimplePGrammar {
          productions :: [PProd]
        , nestedRules :: Map PRule String
    }

instance PGrammar SimplePGrammar where
    _prods  = productions
    _prods' val a = a {productions = val}
    _nested  = nestedRules
    _nested' val a = a {nestedRules = val}
    empty = SimplePGrammar [] M.empty

instance Show SimplePGrammar where
    show = showPGrammar

type SimpleItem = (Int, Int)
instance IsPItem SimpleItem where
    startFrom p    = do
        i <- gets $ (fromJust . (p `L.elemIndex`)) . view prods
        return (i, 0)
    prodIdx (i, _) = return i
    dotPos  (_, j) = return j
    next    (i, j) = do
        l <- gets $ snd . (L.!! i) . view prods
        return $
            if j >= length l
            then Nothing
            else Just $ l !! (j + 1)
    closureLA1 (a, c) = doWhile consuming []
        where
            consuming = [(a, c)]
            doWhile [] x = return x
            doWhile ((pitem, case') : xs) res = do
                maybe' <- next pitem
                let maybe = maybe' >>= \case
                            PTerm _    -> Nothing
                            PNonTerm s -> Just s
                case maybe of
                    Nothing -> doWhile xs res
                    Just s -> do
                        group <- gets $
                                    filter ((== s) . fst) .
                                    view prods
                        group <- mapM startFrom' group
                        error ""
                where
                    startFrom' ::  PGrammar g  => PProd -> State g SimpleItem
                    startFrom' = startFrom




