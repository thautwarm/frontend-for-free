module RBNF.IRs.TransMarisaToReimu where
import           RBNF.IRs.Marisa
import           RBNF.IRs.Reimu
import           RBNF.Name
import           RBNF.Utils
import           Control.Monad.State
import           GHC.Generics (Generic)

import qualified Data.Set as S

marisaToReimu :: State s a -> Marisa -> State s (Reimu a)
marisaToReimu m = (InT <$> m <*>) . f
  where
    frec = marisaToReimu m
    f = \case
        MKAssign n   ir   -> RAssign n <$> frec ir
        MKCall f args -> RCall <$> frec f <*> mapM frec args
        MKAttr   val attr -> RAttr <$> frec val <*> pure attr
        MKPrj    val dim  -> RPrj <$> frec val <*> pure dim
        MKIf cond cla1 cla2 ->
            RIf
                <$> frec cond
                <*> frec cla1
                <*> frec cla2
        MKWhile cond body ->
            RWhile <$> frec cond <*> frec body
        MKDef _ args body -> RDef args <$> frec body
        MKSwitch val cases defau ->
            let
                cases' = forM cases $ \(i, body) ->
                    frec body >>= \body ->
                        frec i >>= \i -> return (i, body)
            in  RSwitch <$> frec val <*> cases' <*> frec defau
        MKBlock suite -> do
            let group defs bs = \case
                    [] -> (reverse defs, reverse bs)
                    def@(MKDef n args body) : xs ->
                        group ((n, def) : defs) bs xs
                    x : xs -> group defs (x : bs) xs
                (defs, bs) = group [] [] suite
                names      = map fst defs
            defs <- mapM (frec . snd) defs
            bs   <- mapM frec bs
            let suite' = defs ++ bs
            return $ RMutual names suite'
        MKVar   n    -> return $ RVar n
        MKInt   i    -> return $ RInt i
        MKStr   s    -> return $ RStr s
        MKBool  b    -> return $ RBool b
        MKTuple elts -> RTuple <$> mapM frec elts
        MKAnd a b    -> RAnd <$> frec a <*> frec b
        MKOr  a b    -> ROr <$> frec a <*> frec b
        MKExtern n t m -> RExtern n t <$> frec m


-- | AIR involves no declarations for local variables.
--   We make 'resolveDecl' to do so.
resolveDecl :: Reimu a -> State (Set MName) (Reimu a)
-- | some sketches
-- t = BBase, f = State (S.Set MName)
-- a = Marisa _
-- f (t a) -> State (S.Set MName) (Marisa a)
-- rslDcl :: Marisa a -> State (S.Set MName) (Marisa a)
--  a <- traverse f (outT rsl)
-- {outT = InT a}

resolveDecl bIR@InT {outT=base} =
  case base of
    RExtern n _ m -> modify (S.insert n) >> resolveDecl m
    RAssign n a ->
      gets (n `S.member`) >>= \case
        False -> do
          modify (S.insert n)
          resumeTag <$> (RDecl n <$> resolveDecl a)
        True  -> resumeTag <$> genericVisit
    RIf a b c -> do
      s <- get
      let (cond, s') = flip runState s  $ resolveDecl a
          tCls  = flip evalState s' $ resolveDecl b
          fCls  = flip evalState s' $ resolveDecl c
      return . resumeTag $ RIf cond tCls fCls

    RWhile a b -> do
      s <- get
      let (cond, s') = flip runState  s  $ resolveDecl a
          body       = flip evalState s' $ resolveDecl b
      return . resumeTag $ RWhile cond body
    RDef args b -> do
      s' <- get
      put $ S.fromList args
      ret <- genericVisit
      put s'
      return . resumeTag $ ret
    RSwitch a bs c -> do
      s <- get
      let (v, s') = flip runState s $ resolveDecl a
          cases   = [(case', flip evalState s' $ resolveDecl b) | (case', b) <- bs]
          defau   = flip evalState s' $ resolveDecl c
      return . resumeTag $ RSwitch v cases defau
    RMutual xs bs -> do
        s <- get
        modify $ S.union (S.fromList xs)
        base <- genericVisit
        let ret = resumeTag base
        put s
        return ret
    _ -> resumeTag <$> genericVisit

  where
    genericVisit = traverse resolveDecl base
    resumeTag  base = bIR {outT = base}