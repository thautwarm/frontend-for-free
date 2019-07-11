{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
module RBNF.Grammar where

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

-- Parsing symbols
data P
    = PTerm Case
    -- PNonTerm symbol is_grammar_node
    | PNonTerm String
    deriving (Eq, Ord)

type PRule = [P]
type PProd = (String, PRule) -- productions
instance Show P where
    show = \case
        PTerm (Case set)  -> "<" ++ showConds set ++ ">"
        PNonTerm s -> s


-- Combinatorial
data C
    = CTerm [MatchCond]
    | CNonTerm String
    | CSeq [C]
    | CAlt [C]
    deriving (Eq, Ord)

type CRule = C
instance Show C where
    show = \case
        CTerm set   -> "<" ++ showConds set ++ ">"
        CNonTerm s  -> s
        CSeq [c]    -> show c
        CSeq cs     -> "(" ++ (concat . L.intersperse " " $ map show cs) ++ ")"
        CAlt [c]    -> show c
        CAlt cs     -> "(" ++ (concat . L.intersperse "|" $ map show cs) ++ ")"

-- Combinatorial
newtype CGrammar = CGrammar (Map String C)

class PGrammar g where
    -- lens infr
    _prods    :: g -> [PProd]
    _prods'   :: [PProd] -> g -> g
    _nested   :: g -> Map PRule String
    _nested'  :: Map PRule String -> g -> g

    -- others
    empty     :: g
    top       :: g -> String
    top       =  fst . head . view prods
    numbering :: PRule -> State g String
    numbering _ =
        gets $ (\x -> "%tmp" ++ show x) . length . view nested

    -- build parsing table
    -- TODO optimise
    firstSet :: P -> State g (Set Case)
    firstSet = \case
        PTerm case' -> return $ S.singleton case'
        PNonTerm s  -> do
            syms <- gets $ map (head . snd) .
                           filter ((== s) . fst) .
                           view prods
            sets <- mapM firstSet syms
            return . S.unions $ sets

prods :: PGrammar g => Lens' g [PProd]
prods f a = (`_prods'` a) <$> f (_prods a)

nested :: PGrammar g => Lens' g (Map PRule String)
nested f a = (`_nested'` a) <$> f (_nested a)

showPGrammar g =
        productions ++ "\n" ++ "nested:\n" ++ tmpNames
        where
            -- why not annotating `join` causes some restrictions?
            join :: Show a => (String, a -> String) -> [a] -> String
            join (sep, show) = L.intercalate sep . map show
            productions      = join("\n", showProd) $ view prods g
            tmpNames         = join("\n", id) $ M.elems (view nested g)
            showProd (sym, rule) = sym ++ " -> " ++ join (" ", show) rule

-- remove all nested rule expressions.
standardizeRule :: PGrammar g => CRule -> State g [PRule]
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
        addTmpRule :: PGrammar g => PRule -> State g P
        addTmpRule [p] = return p
        addTmpRule ps = do
            lookup <- gets $ M.lookup ps . view nested
            case lookup of
                Just sym -> return $ PNonTerm sym
                _ -> do sym <- numbering ps
                        modify $ over nested (M.insert ps sym)
                        modify $ over prods ((sym, ps) :)
                        return $ PNonTerm sym

type MkGrammar g = CGrammar -> g
mkGrammar :: PGrammar g => CGrammar -> g
mkGrammar (CGrammar m) =
    execState procedure emptyState
    where
        emptyState = empty
        procedure :: PGrammar g => State g ()
        procedure = do
            a <- forM (M.toList m) $ \(sym, crule) ->
                do
                    prules <- standardizeRule crule
                    return [(sym, rule) | rule <- prules]
            modify $ over prods (concat a ++)