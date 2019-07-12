{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
module RBNF.Grammar where

import RBNF.Symbols

import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import qualified Data.Array as A
import Control.Monad.State
import Control.Arrow
import Control.Lens (over, view, Lens', makeLenses)

type Array = A.Array

-- Combinatorial
type CGrammar = Set CProd

type PGrammarBuilder = [PProd]

showPGrammarBuilder g =
    productions ++ "\n"
    where
        -- why not annotating `join` causes some restrictions?
        join :: Show a => (String, a -> String) -> [a] -> String
        join (sep, show) = L.intercalate sep . map show
        productions      = join("\n", showProd) $ g
        showProd (sym, rule) = sym ++ " -> " ++ join (" ", show) rule

stackEff :: P -> Int
stackEff = \case
        PTerm _     -> 1
        PNonTerm _  -> 1
        PPack n     -> 1 - n
        PReduce _ n -> 1 - n
        PPred _     -> 0
        PBind   _   -> 1
        PModif  _   -> 0

parsedLength :: PRule -> Int
parsedLength = sum . map stackEff

packStack xs =
    case parsedLength xs of
        0 -> error "... " -- TODO
        1 -> xs
        n -> xs ++ [PPack n]
reduceStack app xs =
    case parsedLength xs of
        0 -> error "... " -- TODO
        n -> xs ++ [PReduce app n]

standardizeRule :: CRule -> State PGrammarBuilder [PRule]
standardizeRule = \case
    CTerm c -> return [[PTerm c]]
    CNonTerm s -> return [[PNonTerm s]]

    CSeq cs -> do
        cs <- mapM standardizeRule cs -- :: [[PRule]]
        return $ map (packStack . concat)
               $ sequence cs

    CAlt cs -> do
        cs <- mapM standardizeRule cs
        return $ concat cs

    -- advanced:
    CBind name c -> do
        prules <- standardizeRule c
        let appendBind :: PRule -> PRule
            appendBind xs = xs ++ [PBind name]
        return $ map appendBind prules
    CPred app   -> return [[PPred app]]
    CModif mdf  -> return [[PModif mdf]]

class HasProductions a where
    _prods  :: a -> Array Int PProd
    _prods' :: Array Int PProd -> a -> a

prods :: HasProductions a => Lens' a (Array Int PProd)
prods f d = (`_prods'` d) <$> f (_prods d)

class HasProdGroups a where
    _prodGroups  :: a -> (Map String [Int])
    _prodGroups' :: (Map String [Int]) -> a -> a
prodGroups :: HasProdGroups a => Lens' a (Map String [Int])
prodGroups f d = (`_prodGroups'` d) <$> f ( _prodGroups d)


class HasLeftRecurs a where
    _leftRecurs  :: a -> Set String
    _leftRecurs' :: Set String -> a -> a
leftRecurs :: HasLeftRecurs a => Lens' a (Set String)
leftRecurs f d = (`_leftRecurs'` d) <$> f ( _leftRecurs d)

groupBy f = M.fromListWith (++) . map (f &&& pure)

inline :: PGrammarBuilder -> PGrammarBuilder
inline g = concatMap inlineProd g
    where groups = M.map (map snd) $ groupBy fst g
          inlineProd :: PProd -> [PProd]
          inlineProd (me, rule) =
            let inlineP :: P -> [PRule]
                inlineP = \case
                    x@(PNonTerm s)
                        | s /= me  -> groups M.! s
                    x              -> [[x]]
            in map (const me &&& id ) $
               map concat             $
               sequence               $
               map inlineP rule

class (HasLeftRecurs g, HasProdGroups g, HasProductions g) => PGrammar g
-- no interfaces

mkGrammar :: CGrammar -> PGrammarBuilder
mkGrammar m =
    inline $ execState procedure []
    where
        procedure :: State PGrammarBuilder ()
        procedure = do
            a <- forM (S.toList m) $ \(sym, crule, reduce) ->
                do
                    prules <- standardizeRule crule
                    let packer =
                            case reduce of
                                Just apply -> reduceStack apply
                                _          -> (++ [PMkSExp sym]) . packStack
                    return [(sym, packer rule) | rule <- prules]
            modify (concat a ++)