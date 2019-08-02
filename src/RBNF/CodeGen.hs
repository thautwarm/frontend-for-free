{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

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

  type err_msg_root = [(int, string)] (* offset, rule name *)

-}

module RBNF.CodeGen where


import RBNF.Symbols
import RBNF.Semantics hiding (CFG)
import RBNF.Graph
import RBNF.LookAHead
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens (view, over)
import qualified Data.Map as M
import qualified Data.List as L



data CompilationInfo
    = CompilationInfo {
          tokenIds   :: Map String Int -- token name to its integer index.
        , graph      :: Graph
        , decisions  :: Map Int (ID3Decision LAEdge Int)
        , withTrace  :: Bool
      }

data AName = AName String | ABuiltin String
  deriving (Show, Eq, Ord)

data ACode
    = AAssign AName ACode -- work in whole fn scope
    | ACall ACode [ACode]
    | AAttr ACode String
    | APrj  ACode Int
    | AIf ACode ACode ACode
    | AWhile ACode ACode
    -- similar to switch but not exactly. the default case will get proceeded
    -- once all cases failed.
    | ASwitch ACode [(Int, ACode)] (Maybe ACode)
    | ADef VName [VName] ACode
    | ALam VName ACode
    | ABlock [ACode]
    -- literal
    | AVar AName
    | AInt Integer
    | AStr String
    | ATuple [ACode]
    | AAnd ACode ACode
    | AOr  ACode ACode
    deriving (Show, Eq, Ord)

dsl_eq         = AVar $ ABuiltin "=="
dsl_mv_forward = AVar $ ABuiltin "move_forward!"
dsl_peekable_n = AVar $ ABuiltin "peekable_n"
dsl_peek_n     = AVar $ ABuiltin "peek_n"
dsl_match_tk   = AVar $ ABuiltin "match_token"
dsl_parse      = AVar $ ABuiltin "parse"

dsl_reset      = AVar $ ABuiltin "reset!"
dsl_cons       = AVar $ ABuiltin "cons"
dsl_nil        = AVar $ ABuiltin "nil"
dsl_null       = AVar $ ABuiltin "null"
dsl_to_errs    = AVar $ ABuiltin "cast_to_errs"
dsl_to_res     = AVar $ ABuiltin "cast_to_result"
dsl_to_any     = AVar $ ABuiltin "cast_to_any"
dsl_mkast      = AVar $ ABuiltin "mk_ast"
dsl_int        = AInt . fromIntegral

tokenId = "idint"
tokenOff = "offset"

slotToStr i = "/slot/" ++ show i
scopedStr scopes s = L.intercalate "/" $ L.reverse (s:scopes)
vNameToStr = \case
  Local s -> s
  Slot  i -> slotToStr i

data CFG
  = CFG {
      slot      :: Int
    , tmp       :: Int
    , tkview    :: ACode
    , offname   :: AName
    , topLevels :: [String]
    , scopes    :: [String]
    , ctx       :: [Map String String]
  }

build :: Monad m => a -> StateT [a] m ()
build a   = modify (a:)


consBlock a (ABlock xs) = ABlock (a:xs)
consBlock a x           = ABlock [a, x]

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


mkSwitch :: CompilationInfo -> ID3Decision LAEdge Int -> StateT [ACode] (State CFG) ()
mkSwitch c@CompilationInfo {
          tokenIds
        , decisions
        , graph
        , withTrace
        } = \case
  ID3Leaf [] -> error "invalid" -- TODO
  ID3Leaf [a] -> codeGen c a
  ID3Leaf xs -> error "Backtracing not supported yet. Try to enlarge K to resolve ambiguities."
  ID3Split k xs -> do
    cfg  <- lift get
    hs_tmp_i <- lift incTmp
    let dsl_tmp_flag_n = AName $ "/tmp/" ++ show hs_tmp_i ++ "/flag"
        dsl_tmp_res_n  = AName $ "/tmp/" ++ show hs_tmp_i ++ "/result"

    let dsl_tokens = tkview cfg
        switch :: [(LAEdge, ID3Decision LAEdge Int)] -> ([(Int, ACode)], Maybe ACode)
        switch = switchImpl [] Nothing
        switchImpl cases default' = \case
          [] -> (cases, default')
          (LAReduce, subDecision):xs ->
              let cont =  block               $
                          flip evalState cfg  $
                          flip execStateT []  $
                          mkSwitch c subDecision
              in  switchImpl cases (Just cont) xs
          (LAShift (Case t), subDecision):xs ->
              let idx   = tokenIds M.! t
                  cont  = block               $
                          flip evalState cfg  $
                          flip execStateT []  $
                          mkSwitch c subDecision
                  case' = (idx, cont)
              in  switchImpl (case':cases) default' xs
        dsl_cur_token = ACall dsl_peek_n [dsl_tokens, dsl_int k]
        dsl_cur_int   = AAttr dsl_cur_token tokenId
        dsl_peekable  = ACall dsl_peekable_n [dsl_tokens, dsl_int k]
    let switch'  = switch xs
        cases    = fst switch'
        default' = snd switch'
        expr     = AIf dsl_peekable
                      (ASwitch dsl_cur_int cases defaultWithFlagAssigned) $
                      if withTrace
                      then ATuple [dsl_int 0, ACall dsl_to_any [dsl_nil]]
                      else dsl_null

        defaultWithFlagAssigned = flip fmap default' . consBlock $ AAssign dsl_tmp_flag_n (dsl_int 1)

    case default' of
        Nothing -> build expr
        Just default_expr -> do
          -- initialize the flag to test whether the default branch has got tried.
          build $ AAssign dsl_tmp_flag_n (dsl_int 0)

          build $ AAssign dsl_tmp_res_n expr

          -- check if any cases success or default branch has got tried.
          -- otherwise we'll have a try on default branch
          build $ let cond1 = if withTrace
                              then ACall dsl_eq [dsl_int 1, APrj (AVar dsl_tmp_res_n) 0]
                              else ACall dsl_eq [dsl_null, AVar dsl_tmp_res_n]
                      cond2 = AVar dsl_tmp_flag_n
                  in  AIf (AOr cond1 cond2)
                          (AVar dsl_tmp_res_n)
                          default_expr


codeGen :: CompilationInfo -> Int -> StateT [ACode] (State CFG) ()
codeGen c@CompilationInfo {
            tokenIds
          , decisions
          , graph
          , withTrace
         } i =
  let node = view nodes graph M.! i in
  case kind node of
    NEntity (ETerm (Case t)) -> do
      cfg <- lift get
      let dsl_tokens     = tkview cfg
          dsl_off_name   = offname cfg
          hs_slot        = slot cfg
          dsl_sloti_n    = AName $ slotToStr hs_slot
      cfg <- lift $ modified (\a -> a {slot = slot a + 1})
      build $ AAssign dsl_off_name (AAttr dsl_tokens tokenOff)
      build $ AAssign dsl_sloti_n  (ACall dsl_match_tk [dsl_tokens, dsl_int $ tokenIds M.! t])
      let cont = block $
                 flip evalState cfg $
                 flip execStateT [] $
                   case (i `M.lookup` decisions, view followed node) of
                    (Just decision, _) -> mkSwitch c decision
                    (_, [followI])     -> codeGen c followI
      build $ let ifErr = if withTrace then
                              let err_hd = ATuple [AVar dsl_off_name, AStr t]
                                  errs   = ACall dsl_to_any [ACall dsl_cons [err_hd, dsl_nil]]
                              in  ATuple [dsl_int 0, errs]
                          else dsl_null
              in  AIf (ACall dsl_eq [AVar dsl_sloti_n, dsl_null])
                      ifErr
                      cont

    NEntity (ENonTerm n) -> do
      cfg <- lift get
      let dsl_tokens     = tkview cfg
          dsl_off_name   = offname cfg
          hs_slot        = slot cfg
          dsl_sloti_chk  = AName $ slotToStr hs_slot ++ "/check"
          dsl_sloti_n    = AName $ slotToStr hs_slot
      cfg <- lift $ modified $ \a -> a {slot = slot a + 1}
      build $ AAssign dsl_off_name (AAttr dsl_tokens tokenOff)
      build $ AAssign dsl_sloti_chk (ACall dsl_parse [AStr n, dsl_tokens])
      let cont = block $
                 flip evalState cfg $
                 flip execStateT [] $
                   case (i `M.lookup` decisions, view followed node) of
                    (Just decision, _) -> mkSwitch c decision
                    (_, [followI])     -> codeGen c followI
          result   = ACall dsl_to_res [APrj (AVar dsl_sloti_chk) 1]
          assSlot  = AAssign dsl_sloti_n result
          ifNotErr = consBlock assSlot cont
      if withTrace then do
           let ifErr    = AVar dsl_sloti_chk
           build $ AIf (ACall dsl_eq [APrj (AVar dsl_sloti_n) 0, dsl_int 0])
                      ifErr
                      ifNotErr
      else build $ AIf (ACall dsl_eq [AVar dsl_sloti_n, dsl_null])
                      dsl_null
                      ifNotErr

irToCode :: IR -> State CFG ACode
irToCode ir = do
  cfg <- get
  let hs_scopes        = scopes cfg
      cur_ctx:ctx_tl   = ctx cfg
      frec = \case
        IRAss (Local s) ir -> do
            i <- incTmp
            let nameStr = scopedStr hs_scopes s ++ "/" ++ show i
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