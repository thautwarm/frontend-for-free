-- | Generating codes through parsing graphs, token names and ID3 decision trees.
-- Author: Taine Zhao(thautwarm)
-- Date: 2019-08-03
-- License: BSD-3-clause
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- Generate codes from the parsing graph and the ID3 decision tree.
-- HOW-TO:
{-
Introduce some builtins:
  type token_view = {tokens : token array, offset :: int}
  peekable_n(token_view, int) -> bool
  peek_n(token_view, int) -> token
  peek(token_view, int) -> token
  move_forward!(token_view) -> unit
  reset!(token_view, int) -> unit
  ...
  type err_msg_root = [(int, string)] (* offset, rule name *)

-}

module RBNF.CodeGen where

import RBNF.Utils
import RBNF.CodeGenIRs.A
import RBNF.CodeGenIRs.ABuiltins
import RBNF.Symbols
import RBNF.Semantics
import RBNF.Graph
import RBNF.LookAHead
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens (view, over)
import qualified Data.Map as M
import qualified Data.List as L



data CompilationInfo
    = CompilationInfo {
        graph      :: Graph
        , decisions  :: Map Int (ID3Decision LAEdge Int)
        , withTrace  :: Bool
        , isLeftRec  :: Bool
      }


slotToStr i = ".slot." ++ show i
scopedStr scopes s = L.intercalate "." $ L.reverse (s:scopes)
vNameToStr = \case
  Local s -> s
  Slot  i -> slotToStr i

data CFG
  = CFG {
      slot      :: Int
    , tmp       :: Int
    , scopes    :: [String]
    , ret       :: AIR
    , isLR      :: Bool
    , ctx       :: [Map String String]
  }

emptyCFG scope = CFG {
      slot    = 0
    , tmp     = 0
    , scopes  = [scope]
    , ret     = dsl_int 0
    , ctx     = [M.empty]
    , isLR    = False
  }

build :: Monad m => a -> StateT [a] m ()
build a   = modify (a:)


consBlock a (ABlock xs) = ABlock (a:xs)
consBlock a x           = ABlock [a, x]

prependBlock xs' (ABlock xs) = ABlock (xs' ++ xs)
prependBlock xs' x           = ABlock $ xs' ++ [x]

block [a] = a
block xs  = ABlock xs

modified f = do
  modify f
  get

incTmp :: State CFG Int
incTmp = do
  s <- get
  put $ s {tmp = tmp s + 1}
  return $ tmp s

runToCode :: CFG -> StateT [AIR] (State CFG) () -> AIR
runToCode cfg = block . L.reverse . flip evalState cfg . flip execStateT []

mkSwitch :: CompilationInfo -> ID3Decision LAEdge Int -> StateT [AIR] (State CFG) ()
mkSwitch c@CompilationInfo {
        decisions
        , graph
        , withTrace
        } = \case
  ID3Leaf [] -> error "invalid" -- TODO
  ID3Leaf [a] -> codeGen c a
  ID3Leaf xs -> error "Backtracing not supported yet. Try to enlarge K to resolve ambiguities."
  ID3Split k xs -> do
    cfg  <- lift get
    hs_tmp_i <- lift incTmp
    let dsl_tmp_flag_n = AName $ ".tmp." ++ show hs_tmp_i ++ ".flag"
        dsl_tmp_res_n  = AName $ ".tmp." ++ show hs_tmp_i ++ ".result"
        failed = if withTrace
                 then ATuple [dsl_false, ACall dsl_to_any [dsl_nil]]
                 else dsl_null
    let dsl_tokens = AVar dsl_tokens_n
        switch :: [(LAEdge, ID3Decision LAEdge Int)] -> ([(AIR, AIR)], AIR)
        switch = switchImpl [] failed
        switchImpl cases default' = \case
          [] -> (cases, default')
          (LAReduce, subDecision):xs ->
              let cont =  runToCode cfg $ mkSwitch c subDecision
              in  switchImpl cases cont xs
          (LAShift t, subDecision):xs ->
              let idx   = ACall dsl_s_to_i [AStr t]
                  cont  = runToCode cfg $ mkSwitch c subDecision
                  case' = (idx, cont)
              in  switchImpl (case':cases) default' xs
        dsl_cur_token = ACall dsl_peek [dsl_tokens, dsl_int k]
        dsl_cur_int   = AAttr dsl_cur_token tokenId
        dsl_la_cond   = ACall dsl_peekable [dsl_tokens, dsl_int k]
    let switch'  = switch xs
        cases    = fst switch'
        default' = snd switch'
        expr     = AIf dsl_peekable
                      (ASwitch dsl_cur_int cases defaultWithFlagAssigned)
                      failed

        defaultWithFlagAssigned = consBlock (AAssign dsl_tmp_flag_n dsl_true) default'
      -- initialize the flag to test whether the default branch has got tried.

    build $ AAssign dsl_tmp_flag_n dsl_false
    build $ AAssign dsl_tmp_res_n expr

    -- check if any cases success or default branch has got tried.
    -- otherwise we'll have a try on default branch
    build $ let cond1 = if withTrace
                        then ACall dsl_eq [dsl_false, APrj (AVar dsl_tmp_res_n) 0]
                        else ACall dsl_eq [dsl_null, AVar dsl_tmp_res_n]
                cond2 = AVar dsl_tmp_flag_n
            in  AIf (AOr cond1 cond2)
                    (AVar dsl_tmp_res_n)
                    default'


codeGen :: CompilationInfo -> Int -> StateT [AIR] (State CFG) ()
codeGen c@CompilationInfo {
          decisions
          , graph
          , withTrace
          , isLeftRec
         } i =
  let node          = view nodes graph M.! i
      getCont i cfg = runToCode cfg $
            case (i `M.lookup` decisions, view followed node) of
            (Just decision, _) -> mkSwitch c decision
            (_, [followI])     -> codeGen c followI
  in
  case kind node of
    NEntity (ETerm t) -> do
      cfg <- lift get
      let dsl_tokens     = AVar dsl_tokens_n
          hs_slot        = slot cfg
          dsl_sloti_n    = AName $ slotToStr hs_slot
      cfg <- lift $ modified (\a -> a {slot = slot a + 1})
      build $ AAssign dsl_off_n (AAttr dsl_tokens tokenOff)
      let tokenId = ACall dsl_s_to_i [AStr t]
      build $ AAssign dsl_sloti_n  (ACall dsl_match_tk [dsl_tokens, tokenId])
      let cont   = getCont i cfg
          err_hd = ATuple [AVar dsl_off_n, AStr t]
          errs   = ACall dsl_to_any [ACall dsl_cons [err_hd, dsl_nil]]
          ifErr
            | withTrace = ATuple [dsl_false, errs]
            | otherwise = dsl_null
      build $ AIf (ACall dsl_eq [AVar dsl_sloti_n, dsl_null])
                    ifErr
                    cont

    NEntity (ENonTerm n) -> do
      cfg <- lift get
      let dsl_tokens     = AVar dsl_tokens_n
          hs_slot        = slot cfg
          dsl_sloti_chk  = AName $ slotToStr hs_slot ++ ".check"
          dsl_sloti_n    = AName $ slotToStr hs_slot
      cfg <- lift $ modified $ \a -> a {slot = slot a + 1}
      -- build $ AAssign dsl_off_n (AAttr dsl_tokens tokenOff)
      let theParser = AVar . AName $ "parse." ++ n
      build $ AAssign dsl_sloti_chk (ACall theParser [dsl_tokens, AVar dsl_globST_n])
      let cont = getCont i cfg
          result
            | withTrace = ACall dsl_to_res [APrj (AVar dsl_sloti_chk) 1]
            | otherwise = AVar dsl_sloti_chk
          assSlot  = AAssign dsl_sloti_n result
          ifNotErr = consBlock assSlot cont
      if withTrace then do
           let ifErr    = AVar dsl_sloti_chk
           build $ AIf (ACall dsl_eq [APrj (AVar dsl_sloti_chk) 0, dsl_false])
                      ifErr
                      ifNotErr
      else build $ AIf (ACall dsl_eq [AVar dsl_sloti_chk, dsl_null])
                      dsl_null
                      ifNotErr
    NEntity (EPredicate ir) -> do
      fn   <- lift $ irToCode ir
      let expr = ACall fn [AVar dsl_globST_n]
      cfg  <- lift get
      let cont = getCont i cfg
      build $
        AIf expr cont $
          if withTrace then
            ATuple [dsl_false, ACall dsl_to_any [dsl_nil]]
          else
            dsl_null
    NEntity (EModify ir) -> do
      sideEffect <- lift $ irToCode ir
      cfg  <- lift get
      let cont = getCont i cfg
      build $ consBlock sideEffect cont
    NEntity (EBind name ir) -> do
      bind <- lift $ irToCode (IRAss (Local name) ir)
      cfg  <- lift get
      let cont = getCont i cfg
      build $ consBlock bind cont
    NEntity (EProc irs) -> do
      stmts <- lift $ mapM irToCode irs
      cfg  <- lift get
      let cont = getCont i cfg
      build $ prependBlock stmts cont
    NEntity (EPushScope s) -> do
      cfg <- lift $ modified $ \cfg ->
        let hd:tl = ctx cfg
        in cfg {scopes = s:scopes cfg, ctx = hd:hd:tl}
      build $ getCont i cfg
    NEntity (EPopScope s) -> do
      cfg <- lift $ modified $ \cfg ->
        let _:tl_ctx   = ctx cfg
            _:tl_scope = scopes cfg
        in cfg {scopes = tl_scope, ctx = tl_ctx}
      build $ getCont i cfg
    NReturn slotId -> do
      expr <- lift $ irToCode $ IRVar (Slot slotId)
      cfg  <- lift $ modified $ \cfg -> cfg {ret = expr} :: CFG
      build $ getCont i cfg
    Stop -> do
      cfg <- lift get
      let CFG {ret} = cfg
      build ret
    LeftRecur | not isLeftRec -> do
      cfg <- lift get
      if isLR cfg then do
        let CFG {ret} = cfg
        build ret
      else do
        let CFG {ret} = cfg
            name = L.head $ scopes cfg
            recn = AName $ "lr." ++ name
        build $ ACall (AVar recn) [ret, AVar dsl_globST_n, AVar dsl_tokens_n]
    LeftRecur | isLeftRec -> do
      cfg <- lift get
      if isLR cfg then do
        let CFG {ret} = cfg
        build ret
      else do
        let name = L.head $ scopes cfg
            cfg'   = cfg {
                  slot = 1
                , tmp  = 0
                , scopes = [name]
                , ctx    = [M.empty]
                , ret    = dsl_int 0
                , isLR   = True
              }
        let cont = getCont i cfg'
            arg0 = AName $ slotToStr 0
            recn = AName $ "lr." ++ name
            try  = AName $ "lr." ++ name ++ ".try"
            cond = if withTrace
                  then ACall dsl_neq [APrj (AVar try) 0, dsl_false]
                  else ACall dsl_neq [AVar try, dsl_null]
            -- fold left recursion:
            -- arg0 <- arg0 a b c ...
            fold = if withTrace
                  then ACall dsl_to_res [APrj (AVar try) 1]
                  else AVar try
            defun = ADef recn [arg0, dsl_globST_n, dsl_tokens_n] $
                      ABlock [
                          AAssign try cont
                        , AWhile cond $ ABlock [
                              AAssign arg0 fold
                            , AAssign try cont
                          ]
                        , AVar arg0
                      ]
        build defun
    Start -> do
      cfg <- lift get
      let name = L.head $ scopes cfg
          fnName = AName $ "parse." ++ name
          cont = getCont i cfg
      build $ ADef fnName [dsl_globST_n, dsl_tokens_n] cont

irToCode :: IR -> State CFG AIR
irToCode ir = do
  cfg <- get
  let hs_scopes        = scopes cfg
      cur_ctx:ctx_tl   = ctx cfg
      frec = \case
        IRAss (Local s) ir -> do
            i <- incTmp
            let nameStr = scopedStr hs_scopes s ++ "." ++ show i
            code <- AAssign (AName nameStr) <$> irToCode ir
            put $ cfg {ctx=M.insert s nameStr cur_ctx:ctx_tl}
            return code
        IRAss (Slot i) ir ->
          AAssign (AName $ slotToStr i) <$> irToCode ir
        IRTuple irs ->
          ATuple <$> mapM irToCode irs
        IRMkSExp n ir -> do
          content <- irToCode ir
          return $ ACall dsl_mkast [AStr n, content]
        IRCall f args ->
          ACall <$> irToCode f <*> mapM irToCode args
        IRVar (Local s) ->
          return $ AVar $ AName $ M.findWithDefault s s cur_ctx
        IRVar (Slot i) ->
          return $ AVar $ AName $ slotToStr i
  frec ir