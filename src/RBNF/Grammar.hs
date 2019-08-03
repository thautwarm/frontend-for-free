{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module RBNF.Grammar where

import RBNF.Symbols

import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Control.Monad.State
import Control.Arrow
import Control.Lens (over, view, Lens', makeLenses)

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
        PMkSExp _ n -> 1 - n
        PPred _     -> 0
        PBind   _   -> 0
        PModif  _   -> 0
        PPushScope _ -> 0
        PPopScope  _ -> 0

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

mkSExpStack name xs =
    case parsedLength xs of
        0 -> error "... " -- TODO
        n -> xs ++ [PMkSExp name n]


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

groupBy f = M.fromListWith (++) . map (f &&& pure)

inline :: PGrammarBuilder -> PGrammarBuilder
inline g = concatMap inlineProd g
    where groups = M.map (map snd) $ groupBy fst g
          inlineProd :: PProd -> [PProd]
          inlineProd (me, rule) =
            let letBlock :: String -> [P] -> [P]
                letBlock s xs = PPushScope s:xs ++ [PPopScope s]
                inlineP :: P -> [PRule]
                inlineP = \case
                    x@(PNonTerm s) | s /= me  ->
                            map (letBlock s) $ groups M.! s
                    x              -> [[x]]
            in map ((const me &&& id) . concat) $
               mapM inlineP rule

mkGrammar :: CGrammar -> PGrammarBuilder
mkGrammar m =
    inline $ execState procedure []
    where
        procedure :: State PGrammarBuilder ()
        procedure = do
            a <- forM (S.toList m) $ \(sym, crule, reduce) ->
                do  prules <- standardizeRule crule
                    let packer =
                            case reduce of
                                Just apply -> reduceStack apply
                                _          -> mkSExpStack sym
                    return [(sym, packer rule) | rule <- prules]
            modify (concat a ++)

data Grammar rhs
    = Grammar {
          _prods  :: Map String [rhs]
        , _leftR  :: Map String [rhs]
    } deriving (Show, Eq, Ord)

makeLenses ''Grammar

collectTokenNames :: PGrammarBuilder -> Set String
collectTokenNames xs =
    flip execState S.empty $ forM_ xs $ collectTokenNamesM . snd