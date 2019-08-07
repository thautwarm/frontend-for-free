-- | HM unification implementations based on propositional logics,
--   based on nominal type system.
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-04
-- License: MIT
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RBNF.CodeGenIRs.HM
where
import RSolve.Logic
import RSolve.Solver
import RSolve.MultiState
import Control.Lens (Lens', view, over, makeLenses)
import Control.Applicative
import Control.Monad

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

type Fix a = a -> a

infixl 6 :->
infixr 6 :*

data T
    = TVar Int
    | TFresh String
    | T :-> T
    | T :*  T -- tuple
    | TForall (S.Set String) T
    | TApp T T -- type application
    | TNom Int -- nominal type index
    deriving (Eq, Ord)

deConsTOp :: T -> Maybe (T -> T -> T, T, T)
deConsTOp = \case
    a :-> b  -> Just ((:->), a, b)
    a :*  b  -> Just ((:*),  a, b)
    TApp a b -> Just (TApp,  a, b)
    _ -> Nothing

instance Show T where
    show = \case
        TVar idx    -> "@" ++ show idx
        TFresh s    -> s
        a :-> b     -> showNest a ++ " -> " ++ show b
        a :*  b     -> showNest a ++ " * " ++ show b
        TForall l t -> "forall " ++ (unwords $ S.toList l) ++ ". " ++ show t
        TApp t1 t2  -> show t1 ++ " " ++ showNest t2
        TNom i       -> "@t" ++ show i
        where
            showNest s
                | isNest s  = "(" ++ show s ++ ")"
                | otherwise = show s
            isNest s = case s of
                TApp _ _    -> True
                TForall _ s -> isNest s
                _ :-> _     -> True
                _ :* _      -> True
                _           -> False

data Unif
    = Unif {
          lhs :: T
        , rhs :: T
        , neq :: Bool
      }
    deriving (Eq, Ord)

instance Show Unif where
    show Unif {lhs, rhs, neq} =
        let op = if neq then " /= " else " == "
        in  show lhs ++ op ++ show rhs

instance AtomF Unif where
    notA a@Unif {neq} = [a {neq = not neq}]

data TCEnv ext = TCEnv {
          _ext   :: ext
        , _noms  :: M.Map Int T  -- nominal type ids
        , _tvars :: M.Map Int T  -- type variables
        , _neqs  :: S.Set (T, T) -- negation constraints
    }
    deriving (Show)

emptyTCEnv ext = TCEnv ext M.empty M.empty S.empty

makeLenses ''TCEnv

newTVar :: MS (TCEnv ext) Int
newTVar = do
    i <- getsMS $ M.size . view tvars
    modifyMS $ over tvars $ M.insert i (TVar i)
    return i

newTNom :: MS (TCEnv ext) Int
newTNom = do
    i <- getsMS $ M.size . view noms
    modifyMS $ over noms $ M.insert i (TNom i)
    return i

loadTVar :: Int -> MS (TCEnv ext) T
loadTVar i = getsMS $ (M.! i) . view tvars

occurIn :: Int -> T -> MS (TCEnv ext) Bool
occurIn l = contains
    where
        contains (deConsTOp -> Just (_, a, b)) = (||) <$> contains a <*> contains b
        contains (TNom _)      = return False
        contains (TForall _ a) = contains a
        contains (TFresh _)    = return False
        contains (TVar a)
            | a == l           = return True
            | otherwise        = do
                tvar <- loadTVar a
                case tvar of
                    TVar a' | a' == a -> return False
                    _                 -> contains tvar

free :: M.Map String T -> T -> T
free m = mkFree
    where
        mkFree (deConsTOp -> Just (op, a, b)) = op (mkFree a) (mkFree b)
        mkFree a@(TNom i)       = a
        mkFree (TForall n t)    = TForall n $ flip free t $ M.withoutKeys m n
        mkFree a@(TVar _)       = a
        mkFree a@(TFresh id)    = M.findWithDefault a id m

freepair freevar = (freevar,) . TVar <$> newTVar

prune :: T -> MS (TCEnv ext) T
prune = \case
        (deConsTOp   -> Just (op, a, b)) -> op <$> prune a <*> prune b
        a@(TNom i)   -> return a
        TVar i       ->
            loadTVar i >>= \case
                a@(TVar  i') | i' == i -> return a
                a                      -> do
                    t <- prune a
                    update i t
                    return t

        a@(TFresh _) -> return a
        TForall a b  -> TForall a <$> prune b

update :: Int -> T -> MS (TCEnv ext) ()
update i t = modifyMS $ over tvars $ M.insert i t

addNEq :: (T, T) -> MS (TCEnv ext) ()
addNEq t = modifyMS $ over neqs (S.insert t)

unify :: Fix (Unif -> MS (TCEnv ext) ())
unify self Unif {lhs, rhs, neq=True} = addNEq (lhs, rhs)

unify self Unif {lhs=TForall freevars poly, rhs} = do
    pairs <- mapM freepair $ S.toList freevars
    let freemap = M.fromList pairs
    let l = free freemap poly
    self Unif {lhs=l, rhs=rhs, neq=False}

unify self a@Unif {lhs, rhs=rhs@(TForall _ _)} =
    self a {lhs=rhs, rhs=lhs}

unify self Unif {lhs=TNom a, rhs=TNom b}
    | a == b      = return ()
    | otherwise   = empty

unify self Unif {lhs=TVar a, rhs = TVar b} = do
    recursive <- occurIn a (TVar b)
    if recursive
    then error "ill formed definition like a = a -> b"
    else update a (TVar b)

unify self Unif {lhs=TVar id, rhs, neq} = update id rhs

unify self a@Unif {lhs, rhs=rhs@(TVar _)} = self a {lhs=rhs, rhs=lhs}

-- type operators are not frist class
unify self Unif {lhs=l1 :-> l2, rhs= r1 :-> r2} =
    self Unif {lhs=l1, rhs=r1, neq=False} >>
    self Unif {lhs=l2, rhs=r2, neq=False}

unify self Unif {lhs=l1 :* l2, rhs= r1 :* r2} =
    self Unif {lhs=l1, rhs=r1, neq=False} >>
    self Unif {lhs=l2, rhs=r2, neq=False}

-- TODO: type aliases?
unify self Unif {lhs=TApp l1 l2, rhs= TApp r1 r2} =
    self Unif {lhs=l1, rhs=r1, neq=False} >>
    self Unif {lhs=l2, rhs=r2, neq=False}

unify self Unif {lhs, rhs} =
    error $ show lhs ++ " ? " ++ show rhs

instance CtxSolver (TCEnv ext) Unif where
    solve =
        let frec = unify (pruneUnif >=> frec)
        in  pruneUnif >=> frec
        where
            pruneUnif a@Unif {neq=True} = return a
            pruneUnif a@Unif {lhs, rhs} = do
                lhs <- prune lhs
                rhs <- prune rhs
                return $ a {lhs=lhs , rhs=rhs}