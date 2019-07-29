{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
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
    | ASwitch ACode [(Int, ACode)]
    | ADef VName [VName] ACode
    | ABlock [ACode]
    deriving (Show, Eq, Ord)

aeq = AVar $ ABuiltin "=="

data CFG = CFG {offset :: Int, tokens :: ACode}


codeGen :: CompilationInfo -> Int -> State CFG ACode
codeGen CompilationInfo {
            tokenNames
          , decisions
          , graph
         } i =
  let node = view nodes graph M.! i in
  case node of
    Node {kind = NEntity (ETerm (Case s))} -> do
      tokensVar <- gets $ tokens
      let idx = tokenNames M.! s
       -- TODO
      return $ AIf (ACall aeq [AInt . fromIntegral $ idx, tokensVar]) aeq aeq



