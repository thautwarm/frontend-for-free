{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module RBNF.Transform where
import RBNF.Symbols
import RBNF.Grammar
import Control.Monad.State
import Control.Lens (makeLenses, Lens', over, view)
import Control.Arrow

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L
import qualified Data.Array as A





data GradualUpdate a = GU {_old :: a, _new :: a}

data SplitByIsLeftRecursive
    = SplitByIsLeftRecursive {_isLeftR :: [PRule], _notLeftR:: [PRule]}

makeLenses ''SplitByIsLeftRecursive
makeLenses ''GradualUpdate

mergeSplited :: [SplitByIsLeftRecursive] -> SplitByIsLeftRecursive
mergeSplited xs = SplitByIsLeftRecursive isLeftR' notLeftR'
                  where
                    extract f = concatMap  (view f) xs
                    isLeftR'  = extract isLeftR
                    notLeftR' = extract notLeftR

isShift :: P -> Bool
isShift = \case
    PNonTerm _ -> True
    PTerm _    -> True
    _          -> False


markedLeftRecur :: PGrammar g => String -> PRule -> State (GradualUpdate g) SplitByIsLeftRecursive
markedLeftRecur root rule =
        let recurs = S.singleton root in
        frec recurs rule
    where
        frec :: PGrammar g => Set String -> PRule -> State (GradualUpdate g) SplitByIsLeftRecursive
        frec recurs = \case
            [] -> error "..." -- TODO: invalid prule
            rule@(PTerm _:xs) -> return $ SplitByIsLeftRecursive [] [rule]
            rule@(PNonTerm name:xs)
                | S.member name recurs ->
                    return $
                    if name == root then SplitByIsLeftRecursive [rule] []
                    else SplitByIsLeftRecursive [] [rule]
                | otherwise -> do
                    let recurs' = S.insert name recurs in
                    do indsOfName <- gets $ (M.! name) . view (old . prodGroups)
                        arr     <- gets $ view (old . prods)
                        mergeSplited <$>
                        mapM (uncurry (frec recurs')) $
                        [snd $ arr A.! ind | ind <- indsOfName]
            x:xs -> do
                    separated <- frec recurs xs
                    let addHd rules = [x : rule | rule <- rules]
                    return $ over isLeftR addHd . over notLeftR addHd $ separated



data CSGProgram
    = CSGPack Int
    | CSGReduce MiniLang Int
    | CSGPred MiniLang
    | CSGBind String
    | CSGModif MiniLang

data CSGS
    = CSGTerm Case
    | CSGNonTerm String
    | CSGProgram
