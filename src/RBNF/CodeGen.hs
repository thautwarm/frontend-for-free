-- | Generating codes through parsing graphs, token names and ID3 decision trees.
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-03
-- License: BSD-3-clause

module RBNF.CodeGen where

import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Maybe                     ( listToMaybe, fromJust )

import           RBNF.Utils
import           RBNF.Name
import           RBNF.Constructs
import           RBNF.Semantics
import           RBNF.Graph
import           RBNF.LookAHead
import           RBNF.IRs.Marisa
import           RBNF.IRs.MarisaLibrary
import           Control.Monad                  ( (>=>) )
import           Debug.Trace                    ( trace )

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.List                     as L

data CompilationInfo
    = CompilationInfo {
        graph      :: Graph
        , decisions  :: Map Int (Decision LAEdge Int)
        , withTrace  :: Bool
        , isLeftRec  :: Bool
        , terminalIds :: [String]
      }


scopedStr scopes s = L.intercalate "." $ L.reverse (s : scopes)

vNameToStr :: Name -> String
vNameToStr = show


-- every function holds it own CFG
data CFG
  = CFG {
    -- e.g., for prod `A ::= b c d`
    --    when `slot = 1`,
    --      we're currently parsing `[b, c, d][1] = c`;
    --    when `slot = 2`,
    --      we're parsing `[b, c, d][2] = d`.
      slot      :: Int
    -- the number of allocated temporary variable
    , tmp       :: Int
    -- the nested scopes in case grammar inline happened.
    -- e.g., for prod group:
    --      A ::= B c
    --      B :: d e
    -- Inline must occur, and B gets eliminated, A becomes
    --      A ::= <push scope B> d e <pop scope> c
    -- , hence, when parsing term `d` or `e`, we shall
    -- have scopes `[..., A, B]`.
    , scopes    :: [String]
    -- The expression to return for current function
    , ret       :: Marisa
    -- is this function to be compiled for handling left recursion?
    -- if true, the slot starts with `1` instead of `0`.
    , isLR      :: Bool
    -- the `(name, manglingName)` pairs for aliased names in current scope.
    , ctx       :: [Map String String]
    -- lookahead cache
    , laCache   :: Map Int String
  }

-- NOTE!!!
-- Don't use this straightforward when compiling left recursion handlers.
emptyCFG scope = CFG { slot    = 0
                     , tmp     = 0
                     , scopes  = [scope]
                     , ret     = dsl_int 0
                     , ctx     = [M.empty]
                     , isLR    = False
                     , laCache = M.empty
                     }

-- reset lookahead cache
reLA :: CFG -> CFG
reLA cfg = cfg { laCache = M.empty }

consumeLA :: CFG -> CFG
consumeLA cfg@CFG { laCache } =
  cfg { laCache = M.mapKeys (\x -> x - 1) laCache }

-- if current terminal is checked
hasLA :: String -> CFG -> Bool
hasLA s cfg@CFG { laCache } = 0 `M.lookup` laCache == Just s

produceLA :: Int -> String -> CFG -> CFG
produceLA i s cfg@CFG { laCache } = cfg { laCache = M.insert i s laCache }

build :: Monad m => a -> StateT [a] m ()
build a = modify (a :)


consBlock a (MKBlock xs) = MKBlock (a : xs)
consBlock a x            = MKBlock [a, x]

prependBlock xs' (MKBlock xs) = MKBlock (xs' ++ xs)
prependBlock xs' x            = MKBlock $ xs' ++ [x]

block [a] = a
block xs  = MKBlock xs

modified f = do
  modify f
  get

incTmp :: State CFG Int
incTmp = do
  s <- get
  put $ s { tmp = tmp s + 1 }
  return $ tmp s


runToCodeSuite :: CFG -> StateT [Marisa] (State CFG) () -> [Marisa]
runToCodeSuite cfg = L.reverse . flip evalState cfg . flip execStateT []

runToCode :: CFG -> StateT [Marisa] (State CFG) () -> Marisa
runToCode cfg = block . runToCodeSuite cfg

mkSuccess :: Bool -> Marisa -> Marisa
mkSuccess withTrace ir | withTrace = MKTuple [MKBool True, ir]
                       | otherwise = ir

-- `mkError` and `mkErrors` work only when `withTrace = True`
mkError :: Marisa -> Marisa -> Marisa
mkError offset error = MKTuple [offset, error]

mkErrors :: [Marisa] -> Marisa
mkErrors xs = MKCall dsl_to_any [mkErrors' xs]
 where
  mkErrors' []       = dsl_nil
  mkErrors' (x : xs) = MKCall dsl_cons [x, mkErrors' xs]

mkSwitch
  :: CompilationInfo -> Decision LAEdge Int -> StateT [Marisa] (State CFG) ()
mkSwitch c@CompilationInfo { decisions, graph, withTrace, terminalIds } = \case
  NDLeaf []        -> error "TODO" -- TODO ?
  NDLeaf [a]       -> codeGen c a
  leaf@(NDLeaf xs) -> do
    cur_scope <- last . scopes <$> lift get
    error
      $  "Found backtracing among nodes "
      ++ show xs
      ++ " at rule "
      ++ cur_scope
      ++ ". Backtracing not supported yet. Try to enlarge K to resolve ambiguities."
  NDSplit k states@(_ : _ : _) xs -> do
    cur_scope <- last . scopes <$> lift get
    error
      $  "Found backtracing among nodes "
      ++ show states
      ++ " at rule "
      ++ cur_scope
      ++ ". Backtracing not supported yet. Try to enlarge K to resolve ambiguities."

  NDSplit k (listToMaybe -> optional) branches -> do
    hs_tmp_i <- lift incTmp
    cfg      <- lift get
    let
      cur_scope     = last $ scopes cfg
      dsl_off_n     = NamedTmp $ ".off." ++ show hs_tmp_i
      dsl_tokens    = MKVar dsl_tokens_n

      la_failed_msg = cur_scope ++ " lookahead failed"
      eof_msg       = cur_scope ++ " got EOF"

      eof_err       = if withTrace
        then MKTuple
          [dsl_false, mkErrors [mkError (MKVar dsl_off_n) (MKStr eof_msg)]]
        else dsl_null
      la_failed = case optional of
        Nothing -> if withTrace
          then
            MKTuple
              [ dsl_false
              , mkErrors [mkError (MKVar dsl_off_n) (MKStr la_failed_msg)]
              ]
          else dsl_null
        Just a -> runToCode cfg $ codeGen c a

      switch :: [(LAEdge, Decision LAEdge Int)] -> ([(Marisa, Marisa)], Marisa)
      switch = switchImpl [] la_failed
      switchImpl cases default' = \case
        [] -> (cases, default')
        (t, subDecision) : xs ->
          let idx   = MKInt $ fromJust (t `L.elemIndex` terminalIds)
              cont  = runToCode (produceLA k t cfg) $ mkSwitch c subDecision
              case' = (idx, cont)
          in  switchImpl (case' : cases) default' xs

      preDecided = \case
        [] -> Nothing
        (t, subDecision) : tl | hasLA t cfg ->
          Just $ runToCode cfg $ mkSwitch c subDecision
        _ : tl -> preDecided tl

      dsl_cur_token = MKCall dsl_peek [dsl_tokens, dsl_int k]
      dsl_cur_int   = MKAttr dsl_cur_token tokenId
      dsl_la_cond   = MKCall dsl_peekable [dsl_tokens, dsl_int k]

      switch'       = switch branches
      cases         = fst switch'
      default'      = snd switch'
      switch_expr   = case preDecided branches of
        Nothing ->
          MKIf dsl_la_cond (MKSwitch dsl_cur_int cases default') eof_err
        Just a -> a

    build $ MKAssign dsl_off_n (MKAttr (MKVar dsl_tokens_n) tokenOff)
    build switch_expr

codeGen :: CompilationInfo -> Int -> StateT [Marisa] (State CFG) ()
codeGen c@CompilationInfo { decisions, graph, withTrace, isLeftRec, terminalIds } i
  = let
      node = view nodes graph M.! i
      getCont i cfg =
        let node = view nodes graph M.! i
        in  runToCode cfg $ case (i `M.lookup` decisions, view followed node) of
              (Just decision, _) -> -- trace (dispID3Tree 0 decision) $
                mkSwitch c decision
              (_, [followI]) -> codeGen c followI
    in
      case kind node of
        DoNothing -> error "node compile error"
        NEntity (ETerm t) -> do
          cfg <- lift get
          let dsl_tokens  = MKVar dsl_tokens_n
              hs_slot     = slot cfg
              dsl_sloti_n = Tmp hs_slot
          if hasLA t cfg
            then do
              build $ MKAssign dsl_sloti_n (MKCall dsl_mv_forward [dsl_tokens])
              cfg <- lift $ modified (\a@CFG { slot } -> a { slot = slot + 1 })
              build $ getCont i $ consumeLA cfg
            else do
              hs_tmp_i <- lift incTmp
              cfg <- lift $ modified (\a@CFG { slot } -> a { slot = slot + 1 })
              let tokenId = MKInt $ fromJust (t `L.elemIndex` terminalIds)
              build $ MKAssign dsl_sloti_n
                               (MKCall dsl_match_tk [dsl_tokens, tokenId])
              let
                cont   = getCont i cfg
                err_hd = MKTuple
                  [MKAttr dsl_tokens tokenOff, MKStr $ t ++ " not match"]
                errs = mkErrors [err_hd]
                ifErr | withTrace = MKTuple [dsl_false, errs]
                      | otherwise = dsl_null
              build $ MKIf (MKCall dsl_is_null [MKVar dsl_sloti_n]) ifErr cont

        NEntity (ENonTerm n) -> do
          cfg <- lift $ modified reLA
          let dsl_tokens    = MKVar dsl_tokens_n
              hs_slot       = slot cfg
              dsl_sloti_chk = NamedTmp $ printf ".check.%d" hs_slot
              dsl_sloti_n   = Tmp hs_slot
          cfg <- lift $ modified $ \a -> a { slot = slot a + 1 }
          -- build $ MKAssign dsl_off_n (MKAttr dsl_tokens tokenOff)
          let theParser = MKVar . NamedTmp $ "parse." ++ n
          build $ MKAssign dsl_sloti_chk
                           (MKCall theParser [MKVar dsl_globST_n, dsl_tokens])
          let cont = getCont i cfg
              result
                | withTrace = MKCall dsl_to_res [MKPrj (MKVar dsl_sloti_chk) 1]
                | otherwise = MKVar dsl_sloti_chk
              assSlot  = MKAssign dsl_sloti_n result
              ifNotErr = consBlock assSlot cont
          if withTrace
            then do
              let ifErr = MKVar dsl_sloti_chk
              build $ MKIf
                (MKCall dsl_eq [MKPrj (MKVar dsl_sloti_chk) 0, dsl_false])
                ifErr
                ifNotErr
            else build $ MKIf (MKCall dsl_is_null [MKVar dsl_sloti_chk])
                              dsl_null
                              ifNotErr

        NEntity (EPred ir) -> do
          let expr = irToCode ir
          cfg <- lift get
          let cont = getCont i cfg
          build $ MKIf expr cont $ if withTrace
            then MKTuple [dsl_false, MKCall dsl_to_any [dsl_nil]]
            else dsl_null

        NEntity (EProc irs) -> do
          let stmts = map irToCode irs
          cfg <- lift get
          let cont = getCont i cfg
          build $ prependBlock stmts cont

        NReturn slotId -> do
          let expr = irToCode $ SVar (Tmp slotId)
          cfg <- lift $ modified $ \cfg -> cfg { ret = expr } :: CFG
          build $ getCont i cfg
        Stop -> do
          cfg <- lift get
          let CFG { ret } = cfg
          -- if not left recursion, no need to check, must be a success
          build $ mkSuccess withTrace ret
        LeftRecur | not isLeftRec -> do
          cfg <- lift get
          if isLR cfg
            then do
              let CFG { ret } = cfg
              build $ mkSuccess withTrace ret
            else do
              let CFG { ret } = cfg
                  name        = L.head $ scopes cfg
                  recn        = NamedTmp $ "lr.loop." ++ name
              build $ MKCall (MKVar recn)
                             [ret, MKVar dsl_globST_n, MKVar dsl_tokens_n]

        LeftRecur | isLeftRec -> do
          cfg <- lift get
          if isLR cfg
            then do -- in `xxx.lr.step`, no need to check
              let CFG { ret } = cfg
              build $ mkSuccess withTrace ret
            else do
              hs_tmp_i <- lift incTmp
              cfg      <- lift get
              let dsl_off_n = NamedTmp $ ".off." ++ show hs_tmp_i
              let name = L.head $ scopes cfg
                  cfg' = cfg { slot   = 1
                             , tmp    = 0
                             , scopes = [name]
                             , ctx    = [M.empty]
                             , ret    = dsl_int 0
                             , isLR   = True
                             }
              let
                cont    = getCont i cfg'
                arg0    = Tmp 0
                rec1    = NamedTmp $ "lr.loop." ++ name
                rec2    = NamedTmp $ "lr.step." ++ name
                try     = NamedTmp $ "lr." ++ name ++ ".try"
                reduced = NamedTmp $ "lr." ++ name ++ ".reduce"
                cond    = if withTrace
                  then MKCall dsl_neq [MKPrj (MKVar try) 0, dsl_false]
                  else MKCall dsl_is_not_null [MKVar try]
                -- fold left recursion:
                -- arg0 <- arg0 a b c ...
                fold = if withTrace
                  then MKCall dsl_to_res [MKPrj (MKVar try) 1]
                  else MKVar try
                args3   = [arg0, dsl_globST_n, dsl_tokens_n]
                params3 = map MKVar [reduced, dsl_globST_n, dsl_tokens_n]
                step    = MKDef rec2 args3 cont
                loop    = MKDef rec1 args3 $ MKBlock
                  [ MKAssign reduced   (MKVar arg0)
                  , MKAssign dsl_off_n (MKAttr (MKVar dsl_tokens_n) tokenOff)
                  , MKAssign try $ MKCall (MKVar rec2) params3
                  , MKWhile cond $ MKBlock
                    [ MKAssign dsl_off_n (MKAttr (MKVar dsl_tokens_n) tokenOff)
                    , MKAssign reduced   fold
                    , MKAssign try $ MKCall (MKVar rec2) params3
                    ]
                  -- following commented expr seems redundant
                  -- , MKCall dsl_reset [MKVar dsl_tokens_n, MKVar dsl_off_n]
                  ,   let cur_off = MKAttr (MKVar dsl_tokens_n) tokenOff in
                      let exitRight | withTrace = MKTuple [MKBool True, MKVar reduced]
                                    | otherwise = MKVar reduced
                      in
                        MKIf (MKCall dsl_eq [cur_off, MKVar dsl_off_n])
                             exitRight  -- left recursion branch exit correctly
                             (MKVar try)
                  ]
              build step
              build loop
        Start -> do
          cfg <- lift get
          let name   = L.head $ scopes cfg
              fnName = NamedTmp $ "parse." ++ name
              cont   = getCont i cfg
          build $ MKDef fnName [dsl_globST_n, dsl_tokens_n] cont

irToCode :: S -> Marisa
irToCode = \case
  SAss n ir -> MKAssign n $ irToCode ir
  STp irs   -> MKTuple $ map irToCode irs
  SExp n ir ->
    let content = irToCode ir in MKCall dsl_mkast [MKStr n, content]
  SCall f args   -> MKCall (irToCode f) (map irToCode args)
  SVar n         -> MKVar n
  SInt i         -> MKInt i
  SAttr s attr   -> MKAttr (irToCode s) attr
  SCombine args  -> MKBlock (map irToCode args)
