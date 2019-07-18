{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module RBNF.Semantics where

import qualified Data.List as L
import qualified Data.Map as M

import RBNF.Symbols
import RBNF.Grammar

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens (over, view, Lens', makeLenses)



data VName =
    Slot Int
  | Local String

instance Show VName where
  show = \case
      Slot i -> "slots["++ show i ++"]"
      Local s -> s

data IR
  = IRAss VName IR
  | IRTuple [IR]
  | IRVar VName
  | forall a. Show a => IRVal a
  | IRMkSExp String IR
  | IRCall IR [IR]
  | IRPushScope
  | IRPopScope

instance Show IR where
  show = \case
      IRAss i ir ->
          show i ++ " <- " ++ show ir
      IRTuple xs ->
          "(" ++ L.intercalate "," (map show xs) ++ ")"
      IRVar n -> show n
      IRVal a -> show a
      IRMkSExp n ir -> n ++ "{" ++ show ir ++ "}"
      IRCall f args ->
          let args_str = "(" ++ L.intercalate "," (map show args) ++ ")"
          in show f ++ args_str
      IRPushScope -> "pushscope"
      IRPopScope  -> "popscope"

type ParsingRoute = [Entity]
data Entity
    = ETerm Case
    | ENonTerm String
    | EPredicate IR

instance Show Entity where
    show = \case
        ETerm c -> show c
        ENonTerm s -> s
        EPredicate p ->
            "pred<" ++ show p ++ ">"

maybeShift = \case
    PTerm c -> Just $ ETerm c
    PNonTerm c -> Just $ ENonTerm c
    _ -> Nothing

data ProgKind
    = ProgNormal
    | ProgPredicate
    | ProgRewrite
    deriving (Show)


data Seman = Seman {
        _route    :: ParsingRoute
      , _prog      :: [(Int, IR, ProgKind)]
    }

makeLenses ''Seman
emptySeman = Seman [] []

indent n s = replicate n ' ' ++ s
instance Show Seman where
    show Seman {_route, _prog} =
        let
            route_Str = unwords $ map (indent 4 . show) _route
            tripleShow :: (Int, IR, ProgKind) -> String
            tripleShow (pos, ir, k) =
                show k ++ " pos " ++
                show pos ++ ", " ++ show ir
            prog_Str = L.intercalate "\n" $
                flip map _prog $
                (indent 4 . tripleShow)
        in "parsing route:\n" ++ route_Str ++ "\n" ++
            "program:\n" ++ prog_Str ++ "\n"


newtype StackObj = SObj Int

data CFG = CFG {
      _pos       :: Int -- >= 0
    , _localN    :: Int -- < 0
    , _stack     :: [StackObj]
}

makeLenses ''CFG
emptyCFG = CFG 0 (-1) []

newObj :: State CFG StackObj
newObj = do
    i <- gets $ view localN
    modify $ over localN (-1 +)
    return $ SObj i

shiftReduce :: State CFG StackObj
shiftReduce = do
    i <- gets $ view pos
    modify $ over pos (+ 1)
    return $ SObj i

bindName :: String -> IR -> IR
bindName s = IRAss (Local s)

push :: StackObj -> State CFG ()
push obj =
    modify $ over stack (obj:)

pop :: State CFG StackObj
pop = do
    stack' <- gets $ view stack
    let hd:tl = stack'
    modify $ over stack $ const tl
    return hd

irOfObj (SObj i) = IRVar $ Slot i
refObj (SObj iL) = IRAss (Slot iL)

addProg pos ir kind = over prog ((pos, ir, kind):)

miniLangToIR = \case
    MTerm s -> IRVar $ Local s
    MApp f args ->
        let fn = IRVar $ Local f
            args' = map miniLangToIR args
        in IRCall fn args'


analyse' :: Seman -> [P] -> State CFG Seman
analyse' seman = \case
    [] -> return $ seman
    (maybeShift -> Just x):xs -> do
        obj <- shiftReduce
        push obj
        seman <- analyse' seman xs
        return $ over route (x:) seman
    PPack n:xs -> do
        tp <- IRTuple . reverse . map irOfObj <$> replicateM n pop
        obj <- newObj
        push obj
        pos' <- gets $ view pos
        let seman' = addProg pos' (refObj obj tp) ProgNormal seman
        analyse' seman' xs
    PBind s:xs -> do
        obj@(SObj i) <- pop
        pos' <- gets $ view pos
        let ir     = irOfObj obj
            seman' = addProg pos' (bindName s ir) ProgNormal seman
        push obj
        analyse' seman' xs
    PMkSExp s n:xs -> do
        tp <- IRTuple . reverse . map irOfObj <$> replicateM n pop
        obj <- newObj
        push obj
        pos' <- gets $ view pos
        let ir    = IRMkSExp s tp
            seman' = addProg pos' (refObj obj ir) ProgNormal seman
        analyse' seman' xs
    PModif m:xs -> do
        pos' <- gets $ view pos
        let seman' = addProg pos' (miniLangToIR m) ProgNormal seman
        analyse' seman' xs
    PPred m:xs -> do
        seman <- analyse' seman xs
        let predProg =  miniLangToIR m
        return $ over route (EPredicate predProg:) seman
    PReduce m n:xs -> do
        tp  <- IRTuple . reverse . map irOfObj <$> replicateM n pop
        obj <- newObj
        push obj
        pos' <- gets $ view pos
        let fn   = miniLangToIR m
            call = IRCall fn [tp]
            seman' = addProg pos' (refObj obj call) ProgRewrite seman
        analyse' seman' xs
    PPushScope:xs -> do
        pos' <- gets $ view pos
        flip analyse' xs $ addProg pos' IRPushScope ProgNormal seman
    PPopScope:xs -> do
        pos' <- gets $ view pos
        flip analyse' xs $ addProg pos' IRPopScope ProgNormal seman


analyse = analyse' emptySeman
pGToSG :: Grammar [P] -> Grammar Seman
pGToSG g =
    let transf lens initCFG =
            let f = map $ flip evalState initCFG . analyse
            in  M.map f $ view lens g
        prods' = transf prods emptyCFG
        leftR' = transf leftR emptyCFG
    in Grammar prods' leftR'
