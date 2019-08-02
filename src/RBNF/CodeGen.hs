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

data CompilationInfo
    = CompilationInfo {
          tokenIds   :: Map String Int -- token name to its integer index.
        , graph      :: Graph
        , decisions  :: Map Int (ID3Decision LAEdge Int)
      }

data AName = ALocal String | ASlot Int | ABuiltin String
  deriving (Show, Eq, Ord)

data ACode
    = ALocalBind AName ACode -- work inside the nearest ABlock
    | AAssign AName -- work in whole fn scope
    | ACall ACode [ACode]
    | AAttr ACode String
    | APrj  ACode ACode
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
    deriving (Show, Eq, Ord)



dsl_eq         = AVar $ ABuiltin "=="
dsl_mv_forward = AVar $ ABuiltin "move_forward!"
dsl_peekable_n = AVar $ ABuiltin "peek_n"
dsl_peek_n     = AVar $ ABuiltin "peek_n"
dsl_reset      = AVar $ ABuiltin "reset!"
dsl_cons       = AVar $ ABuiltin "cons"
dsl_null       = AVar $ ABuiltin "null"
dsl_int        = AInt . fromIntegral

tokenId = "idint"


data CFG
  = CFG {
      slot      :: Int
    , tkview    :: ACode
    , topLevels :: [String]
    , scopes    :: [Map String String]
  }

mkSwitch :: CompilationInfo -> ID3Decision LAEdge Int -> State CFG ACode
mkSwitch c@CompilationInfo {
          tokenIds
        , decisions
        , graph
        } = \case
  ID3Leaf [] -> error "invalid" -- TODO
  ID3Leaf [a] -> codeGen c a
  ID3Leaf xs -> error "Backtracing not supported yet. Try enlarge K to resolve ambiguities."
  ID3Split k xs -> do
    cfg <- get
    let dsl_tokens = tkview cfg
        hs_slot    = slot   cfg

        switch :: [(LAEdge, ID3Decision LAEdge Int)] -> ([(Int, ACode)], Maybe ACode)
        switch = switchImpl [] Nothing
        switchImpl cases default' = \case
          [] -> (cases, default')
          (LAReduce, subDecision):xs ->
              let cont = flip evalState cfg $ mkSwitch c subDecision
              in  switchImpl cases (Just cont) xs
          (LAShift (Case t), subDecision):xs ->
              let idx   = tokenIds M.! t
                  cont  = flip evalState cfg $ mkSwitch c subDecision
                  case' = (idx, cont)
              in  switchImpl (case':cases) default' xs
        dsl_cur_token = ACall dsl_peek_n [dsl_tokens, dsl_int k]
        dsl_cur_int   = AAttr dsl_cur_token tokenId
        dsl_peekable  = ACall dsl_peekable_n [dsl_tokens, dsl_int k]

    return $
      AIf dsl_peekable
          (uncurry (ASwitch dsl_cur_int) $ switch xs)
          dsl_null

codeGen :: CompilationInfo -> Int -> State CFG ACode
codeGen c@CompilationInfo {
            tokenIds
          , decisions
          , graph
         } i =
  let node = view nodes graph M.! i in
  case kind node of
    NEntity (ETerm (Case t)) -> error ""