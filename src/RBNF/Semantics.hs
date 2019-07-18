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
      Slot i -> "local["++ show i ++"]"
      Local s -> s

data IR
  = IRAss VName IR
  | IRTuple [IR]
  | IRVar VName
  | forall a. Show a => IRVal a
  | IRMkSExp String IR
  | IRCall IR [IR]

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

data Shiftable
    = STerm Case
    | SNonTerm String

instance Show Shiftable where
    show = \case
        STerm c -> show c
        SNonTerm s -> s

maybeShift = \case
    PTerm c -> Just $ STerm c
    PNonTerm c -> Just $ SNonTerm c
    _ -> Nothing

data ProgKind
    = ProgNormal
    | ProgPredicate
    | ProgRewrite
    deriving (Show)

data Seman = Seman {
        _shifts    :: [Shiftable]
      , _prog      :: [(Int, IR, ProgKind)]
      , bounds     :: Map String Int
    }

indent n s = replicate n ' ' ++ s
instance Show Seman where
    show Seman {_shifts, _prog, bounds} =
        let
            shifts_Str = unwords $ map (indent 4 . show) _shifts
            tripleShow :: (Int, IR, ProgKind) -> String
            tripleShow (pos, ir, k) =
                show k ++ " pos " ++
                show pos ++ ", " ++ show ir
            prog_Str = L.intercalate "\n" $
                flip map _prog $
                (indent 4 . tripleShow)
        in "shift reduce terms:\n" ++ shifts_Str ++ "\n" ++
            "program:\n" ++ prog_Str ++ "\n" ++
            show bounds

makeLenses ''Seman

newtype StackObj = SObj Int

data CFG = CFG {
      _pos       :: Int  -- >= 0
    , _localN    :: Int -- < 0
    , _stack     :: [StackObj]
    , _locals    :: Map String Int
}

makeLenses ''CFG

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
    [] -> do
        locals' <- gets $ view locals
        return $ seman {bounds = locals'}
    (maybeShift -> Just x):xs -> do
        obj <- shiftReduce
        push obj
        seman <- analyse' seman xs
        return $ over shifts (x:) seman
    PPack n:xs -> do
        elts <- replicateM n pop
        let tp = IRTuple . reverse . map irOfObj $ elts
        obj <- newObj
        push obj
        pos' <- gets $ view pos
        let seman' = addProg pos' (refObj obj tp) ProgNormal seman
        analyse' seman' xs
    PBind s:xs -> do
        obj@(SObj i) <- pop
        modify $ over locals (M.insert s i)
        push obj
        analyse' seman xs
    PMkSExp s:xs -> do
        component@(SObj i) <- pop
        obj <- newObj
        push obj
        pos' <- gets $ view pos
        let ast    =  IRMkSExp s (irOfObj component)
            seman' = addProg pos' (refObj obj ast) ProgNormal seman
        analyse' seman' xs
    PModif m:xs -> do
        pos' <- gets $ view pos
        let seman' = addProg pos' (miniLangToIR m) ProgNormal seman
        analyse' seman' xs
    PPred m:xs -> do
        pos' <- gets $ view pos
        let seman' = addProg pos' (miniLangToIR m) ProgPredicate seman
        analyse' seman' xs
    PReduce m n:xs -> do
        replicateM_ n pop
        pos' <- gets $ view pos
        let seman' = addProg pos' (miniLangToIR m) ProgRewrite seman
        analyse' seman' xs

analyse = analyse' $ Seman [] [] M.empty
pGToSG :: Grammar [P] -> Grammar Seman
pGToSG g =
    let emptyCFGForLR  = CFG 1 (-1) [SObj 0] M.empty
        emptyCFGNormal = CFG 0 (-1) [] M.empty
        transf lens initCFG =
                let f = map $ flip evalState initCFG . analyse
                in  M.map f $ view lens g
        prods' = transf prods emptyCFGNormal
        leftR' = transf leftR emptyCFGForLR
    in Grammar prods' leftR'
