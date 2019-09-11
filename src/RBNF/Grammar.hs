module RBNF.Grammar where

import RBNF.Symbols
import RBNF.Utils

import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.List  as L
import Control.Monad.State
import Control.Arrow

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


standardizeRoot :: CRule -> State PGrammarBuilder [PRule]
standardizeRoot = \case
    CSeq cs -> do
        cs <- mapM standardizeRoot cs -- :: [[PRule]]

        return $ map concat
               $ sequence cs
    CAlt cs -> do
        cs <- mapM standardizeRoot cs
        return $ concat cs
    a -> standardizeRule a

standardizeRule :: CRule -> State PGrammarBuilder [PRule]
standardizeRule = \case
    CTerm c -> return [[PTerm c]]
    CNonTerm s -> return [[PNonTerm s]]

    CSeq cs -> do
        cs <- mapM standardizeRule cs -- :: [[PRule]]
        return $ map (packStack . concat)
               $ sequence cs
    COpt c -> ([]:) <$> standardizeRule c -- :: [PRule]
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

mkGrammar :: CGrammar -> PGrammarBuilder
mkGrammar m =
    execState procedure []
    where
        procedure :: State PGrammarBuilder ()
        procedure = do
            a <- forM (L.nub $ getCGrammar m) $ \(sym, crule, reduce) ->
                do  prules <- standardizeRoot crule
                    let packer =
                            case reduce of
                                Just apply -> -- trace (show apply) $
                                    reduceStack apply
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