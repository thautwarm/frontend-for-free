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
type CGrammar = Map String C

data PGrammarBuilder
    = PGrammarBuilder {
          prods  :: [PProd]
        , nested :: Map PRule String
    }
    deriving (Show, Eq, Ord)

prods' :: Lens' PGrammarBuilder [PProd]
prods' f d = (\val -> d {prods = val}) <$> f (prods d)

nested' :: Lens' PGrammarBuilder (Map PRule String)
nested' f d = (\val -> d {nested = val}) <$> f (nested d)

emptyPGrammarBuilder :: PGrammarBuilder
emptyPGrammarBuilder = PGrammarBuilder [] M.empty

numbering :: PRule -> State PGrammarBuilder String
numbering  _ = gets $ (\x -> "%tmp" ++ show x) . length . nested

showPGrammarBuilder g =
        productions ++ "\n" ++ "nested:\n" ++ tmpNames
        where
            -- why not annotating `join` causes some restrictions?
            join :: Show a => (String, a -> String) -> [a] -> String
            join (sep, show) = L.intercalate sep . map show
            productions      = join("\n", showProd) $ prods g
            tmpNames         = join("\n", id) $ M.elems (nested g)
            showProd (sym, rule) = sym ++ " -> " ++ join (" ", show) rule


standardizeRule :: CRule -> State PGrammarBuilder [PRule]
standardizeRule = \case
    CTerm conds ->
        let condset = S.fromList conds
        in return [[PTerm $ Case condset]]
    CNonTerm s -> return [[PNonTerm s]]

    CSeq cs -> do
        cs <- mapM standardizeRule cs
        sequence [mapM addTmpRule sections | sections <- sequence cs]

    CAlt cs -> do
        cs <- mapM standardizeRule cs
        return $ concat cs
    where
        addTmpRule :: PRule -> State PGrammarBuilder P
        addTmpRule [p] = return p
        addTmpRule ps = do
            lookup <- gets $ M.lookup ps . nested
            case lookup of
                Just sym -> return $ PNonTerm sym
                _ -> do sym <- numbering ps
                        modify $ over nested' (M.insert ps sym)
                        modify $ over prods' ((sym, ps) :)
                        return $ PNonTerm sym



class PGrammar a where
    productions  :: a ->  Array Int PProd
    tmpNames     :: a -> Set String
    prodGroups   :: a -> Map String Int
    _leftRecurs  :: a -> Map String [PRule]
    _leftRecurs' :: Map String [PRule] -> a -> a

leftRecurs :: PGrammar a => Lens' a (Map String [PRule])
leftRecurs f d = (`_leftRecurs'` d) <$> f ( _leftRecurs d)

mkGrammar :: CGrammar -> PGrammarBuilder
mkGrammar m =
    execState procedure emptyPGrammarBuilder
    where
        procedure :: State PGrammarBuilder ()
        procedure = do
            a <- forM (M.toList m) $ \(sym, crule) ->
                do
                    prules <- standardizeRule crule
                    return [(sym, rule) | rule <- prules]
            modify $ over prods' (concat a ++)

