module RBNF.Grammar where

import           RBNF.Constructs
import           RBNF.Utils

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Control.Monad.State
import           Control.Arrow

stackEff :: P -> Int
stackEff = \case
    PTerm    _   -> 1
    PNonTerm _   -> 1
    PPack    n   -> 1 - n
    PReduce _ n  -> 1 - n
    PMkSExp _ n  -> 1 - n
    PBind      _ -> 0
    PPushScope _ -> 0
    PPopScope  _ -> 0

parsedLength :: [P] -> Int
parsedLength = sum . map stackEff

packStack xs = case parsedLength xs of
    0 -> error "... " -- TODO
    1 -> xs
    n -> xs ++ [PPack n]

reduceStack app xs = case parsedLength xs of
    0 -> error "... " -- TODO
    n -> xs ++ [PReduce app n]

mkSExpStack name xs = case parsedLength xs of
    0 -> error "... " -- TODO
    n -> xs ++ [PMkSExp name n]


standardizeRoot :: C -> [[P]]
standardizeRoot = \case
    CSeq cs -> map concat $ sequence $ map standardizeRoot cs

    CAlt cs -> cs >>= standardizeRoot

    a       -> standardizeRule a

standardizeRule :: C -> [[P]]
standardizeRule = \case
    CTerm    c   -> [[PTerm c]]
    CNonTerm s   -> [[PNonTerm s]]

    CSeq     cs  -> map (packStack . concat) $ sequence $ map standardizeRule cs

    COpt     c   -> [PPack 0] : standardizeRule c
    CAlt     cs  -> cs >>= standardizeRule

    -- advanced:
    CBind name c -> do
        ps <- standardizeRule c
        return $ ps ++ [PBind name]

unCombinatorial :: [CProd] -> [PProd]
unCombinatorial cs = L.nub $ L.nub cs >>= \(sym, c, action) ->
    let packer | Just apply <- action = reduceStack apply
               | otherwise            = mkSExpStack sym
    in  do
            ps <- standardizeRoot c
            return (sym, packer ps)

data Grammar rhs
    = Grammar {
          _prods  :: Map String [rhs]
        , _leftR  :: Map String [rhs]
    } deriving (Show, Eq, Ord)

makeLenses ''Grammar
