-- | HM unification implementations based on propositional logics,
--   based on nominal type system.
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-04
-- License: MIT
{-# LANGUAGE TupleSections #-}
module RBNF.HMTypeInfer
where
import RSolve.Logic
import RSolve.Solver
import RSolve.MultiState
import Control.Lens (Lens', view, over, makeLenses)
import Control.Applicative
import Control.Monad
import GHC.Generics (Generic)
-- import Debug.Trace

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

type Fix a = a -> a

infixl 6 :->
infixr 6 :*
infixr 5 :#

-- | Hindley-milner type
data HMT nom
    = TVar Int
    | TFresh String
    | HMT nom :-> HMT nom
    | HMT nom :*  HMT nom -- tuple
    | HMT nom :# HMT nom -- type application
    | TForall (S.Set String) (HMT nom)
    | TNom nom -- nominal type index
    deriving (Eq, Ord, Generic)

deConsTOp :: HMT nom -> Maybe (HMT nom -> HMT nom -> HMT nom, HMT nom, HMT nom)
deConsTOp = \case
    a :-> b  -> Just ((:->), a, b)
    a :*  b  -> Just ((:*),  a, b)
    a :#  b  -> Just ((:#),  a, b)
    _ -> Nothing

instance Show nom => Show (HMT nom) where
    show = \case
        TVar idx    -> "@" ++ show idx
        TFresh s    -> s
        a :-> b     -> showNest a ++ " -> " ++ show b
        a :*  b     -> showNest a ++ " * " ++ show b
        a :#  b     -> show a ++ " " ++ showNest b
        TForall l t -> "forall " ++ (unwords $ S.toList l) ++ ". " ++ show t
        TNom i      -> show i
        where
            showNest s
                | isNest s  = "(" ++ show s ++ ")"
                | otherwise = show s
            isNest s = case s of
                (deConsTOp -> Just _) -> True
                TForall _ s           -> isNest s
                _                     -> False

data HMUnif nom
    = Unif {
          lhs :: HMT nom
        , rhs :: HMT nom
        , neq :: Bool
      }
    deriving (Eq, Ord)

instance Show nom => Show (HMUnif nom) where
    show Unif {lhs, rhs, neq} =
        let op = if neq then " /= " else " == "
        in  show lhs ++ op ++ show rhs

instance (Ord nom, Show nom) => AtomF (HMUnif nom) where
    notA a@Unif {neq} = [a {neq = not neq}]

data HMTCEnv nom ext = TCEnv {
          _ext   :: ext
        , _tvars :: M.Map Int (HMT nom)  -- type variables
        , _neqs  :: S.Set (HMT nom, HMT nom) -- negation constraints
    }
    deriving (Show)

emptyTCEnv ext = TCEnv ext M.empty S.empty

makeLenses ''HMTCEnv

newTVar :: Ord nom => MS (HMTCEnv nom ext) Int
newTVar = do
    i <- getsMS $ M.size . view tvars
    modifyMS $ over tvars $ M.insert i (TVar i)
    return i

loadTVar :: Ord nom => Int -> MS (HMTCEnv nom ext) (HMT nom)
loadTVar i = getsMS $ (M.! i) . view tvars

occurIn :: Ord nom => Int -> HMT nom -> MS (HMTCEnv nom ext) Bool
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

free :: M.Map String (HMT nom) -> (HMT nom) -> (HMT nom)
free m = mkFree
    where
        mkFree (deConsTOp -> Just (op, a, b)) = op (mkFree a) (mkFree b)
        mkFree a@(TNom i)       = a
        mkFree (TForall n t)    = TForall n $ flip free t $ M.withoutKeys m n
        mkFree a@(TVar _)       = a
        mkFree a@(TFresh id)    = M.findWithDefault a id m

freepair freevar = (freevar,) . TVar <$> newTVar

prune :: Ord nom => HMT nom -> MS (HMTCEnv nom ext) (HMT nom)
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

update :: Ord nom => Int -> HMT nom -> MS (HMTCEnv nom ext) ()
update i t = modifyMS $ over tvars $ M.insert i t

addNEq :: Ord nom => (HMT nom, HMT nom) -> MS (HMTCEnv nom ext) ()
addNEq t = modifyMS $ over neqs (S.insert t)

unify :: (Show nom, Ord nom) => Fix (HMUnif nom -> MS (HMTCEnv nom ext) ())
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

unify self Unif {lhs=TVar a, rhs = TVar b}
    | a == b = return ()
    | otherwise = do
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

-- TODO: how to support type aliases?
unify self Unif {lhs=l1 :# l2, rhs= r1 :# r2} =
    self Unif {lhs=l1, rhs=r1, neq=False} >>
    self Unif {lhs=l2, rhs=r2, neq=False}

unify self Unif {lhs, rhs} =
    error $ show lhs ++ " ? " ++ show rhs

instance (Show nom, Ord nom) => CtxSolver (HMTCEnv nom ext) (HMUnif nom) where
    solve =
        let frec = unify (pruneUnif >=> frec)
        in  pruneUnif >=> frec
        where
            pruneUnif a@Unif {neq=True} = return a
            pruneUnif a@Unif {lhs, rhs} = do
                lhs <- prune lhs
                rhs <- prune rhs
                return $ {- trace (show lhs ++ " unify " ++ show rhs) $ -} a {lhs=lhs , rhs=rhs}