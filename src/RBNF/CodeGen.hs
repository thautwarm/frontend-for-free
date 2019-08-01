{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
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
          tokenNames :: Map String Int -- token name to its integer index.
        , graph      :: Graph
        , decisions  :: Map Int (ID3Decision LAEdge Int)
      }

data AName = ALocal String | ASlot Int | ABuiltin String
  deriving (Show, Eq, Ord)
data ACode
    = ALet AName ACode
    | AVar AName
    | AInt Integer
    | ATuple [ACode]
    | ACall ACode [ACode]
    | AAttr ACode String
    | APrj  ACode ACode
    | AIf ACode ACode ACode
    | AWhile ACode ACode
    | ASwitch ACode [(Int, ACode)] (Maybe ACode)
    | ADef VName [VName] ACode
    | ABlock [ACode]
    deriving (Show, Eq, Ord)


aeq = AVar $ ABuiltin "=="
amoveForward = AVar $ ABuiltin "move_forward"
amoveForward' = AVar $ ABuiltin "move_forward!"
apeek0 = AVar $ ABuiltin "peek0"
apeekN = AVar $ ABuiltin "peekN"
tokenId = "idint"
aint = AInt . fromIntegral
data CFG
  = CFG {
      offset :: Int
    , tokens :: ACode
    , topLevels :: [String]
  }

mkSwitch :: CompilationInfo -> ID3Decision LAEdge Int -> State CFG ACode
mkSwitch c@CompilationInfo {
          tokenNames
        , decisions
        , graph
        } = \case
  ID3Leaf [] -> error "invalid" -- TODO
  ID3Leaf [a] -> codeGen c a
  ID3Leaf xs -> error "Backtracing not supported yet. Try enlarge K to resolve conflicts."
  ID3Split k xs -> do
    tokensVar <- gets tokens
    tokenOffset <- gets offset
    cfg <- get
    let f :: [(LAEdge, ID3Decision LAEdge Int)] -> ([(Int, ACode)], Maybe ACode)
        f = fImpl [] Nothing
        fImpl :: [(Int, ACode)]-> Maybe ACode -> [(LAEdge, ID3Decision LAEdge Int)] -> ([(Int, ACode)], Maybe ACode)
        fImpl cases default' = \case
          [] -> (cases, default')
          (LAReduce, subDecision):xs ->
              let cont = flip evalState cfg $ mkSwitch c subDecision
              in  fImpl cases (Just cont) xs
          (LAShift (Case t), subDecision):xs ->
              let idx   = tokenNames M.! t
                  cont  = flip evalState cfg $ mkSwitch c subDecision
                  case' = (idx, cont)
              in  fImpl (case':cases) default' xs
        curToken = ACall apeekN [tokensVar, aint tokenOffset]
        curInt   = AAttr curToken tokenId
    return $ uncurry (ASwitch curInt) $ f xs

codeGen :: CompilationInfo -> Int -> State CFG ACode
codeGen CompilationInfo {
            tokenNames
          , decisions
          , graph
         } i =
  let node = view nodes graph M.! i in
  case node of
    Node {kind = NEntity (ETerm (Case s))} -> do
      tokensVar <- gets tokens
      tokenOffset <- gets offset
      let idx      = tokenNames M.! s
          curToken = ACall apeekN [tokensVar, aint tokenOffset]
          curInt   = AAttr curToken tokenId
          cmp      = ACall aeq [aint idx, curInt]
          nexts    = view followed node
       -- TODO
      return $ AIf (ACall aeq [AInt . fromIntegral $ idx, tokensVar]) aeq aeq


