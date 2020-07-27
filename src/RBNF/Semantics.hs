module RBNF.Semantics
    ( ParsingRoute
    , pGToSG
    , Entity(..)
    , Seman(..)
    , route
    , S(..)
    )
where

import qualified Data.List                     as L
import qualified Data.Map                      as M

import           RBNF.Constructs
import           RBNF.Grammar
import           RBNF.Name
import           RBNF.Utils

import           Control.Monad.State
import           Control.Monad.Reader
import           Debug.Trace (trace)

-- Stack VM
data S
  = SAss Name S
  | STp [S]
  | SVar Name
  | SExp String S
  | SInt Int
  | SCall S [S]
  | SCombine [S]
  | SAttr S String
  deriving (Eq, Ord, Show)

type ParsingRoute = [Entity]
data Entity
    = ETerm String
    | ENonTerm String
    | EProc [S]
    | EPred S
    deriving (Eq, Ord)

instance Show Entity where
    show = \case
        ETerm    c   -> c
        ENonTerm s   -> s
        EProc    irs -> L.intercalate ";" $ map show irs

maybeShift = \case
    PTerm    c -> Just $ ETerm c
    PNonTerm c -> Just $ ENonTerm c
    _          -> Nothing

data Seman
    = Seman {
        _route    :: ParsingRoute
      , ret       :: Int
    }
    deriving (Eq, Ord)

makeLenses ''Seman
emptySeman = Seman [] 0

instance Show Seman where
    show Seman { _route, ret } =
        let route_Str = unwords $ map (indent 4 . show) _route
        in  "parsing route:\n"
                ++ route_Str
                ++ "\n"
                ++ "return: "
                ++ show ret
                ++ "\n"

newtype StackObj = SObj Int deriving (Show)

data CFG = CFG {
      _pos       :: Int -- >= 0
    , _localN    :: Int -- < 0
    , _stack     :: [StackObj]
    , _scopes     :: [(String, Map String StackObj)]
}

makeLenses ''CFG
emptyCFG = CFG 0 (-1) [] [("non-nested", M.empty)]

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
push obj = modify $ over stack (obj :)

pop :: State CFG StackObj
pop = do
    stack' <- gets $ view stack
    let hd : tl = stack'
    modify $ over stack $ const tl
    return hd

peek0 :: State CFG StackObj
peek0 = gets $ (head . view stack)

popScope :: State CFG (String, Map String StackObj)
popScope = do
    ret <- gets $ L.head . view scopes
    modify $ over scopes L.tail
    return ret



pushScope :: String -> Map String StackObj -> State CFG ()
pushScope n scope = modify $ over scopes ((n, scope) :)


enter :: String -> StackObj -> State CFG ()
enter n o = do
    (scopeName, scope) <- popScope
    case M.lookup n scope of
        Just _ -> error $ printf "duplicate name %s in %s" n scopeName
        _      -> pushScope scopeName $ M.insert n o scope


require :: String -> State CFG (Maybe StackObj)
require n = do
    scope <- gets $ snd . L.head . view scopes
    return $ M.lookup n scope

irOfObj (SObj i) = SVar $ Tmp i
refObj (SObj iL) = SAss (Tmp iL)


listIndexMaybe :: Int -> [a] -> a
listIndexMaybe 0 (x:xs) = x
listIndexMaybe n [] = error $ "out of bound: " ++ show n
listIndexMaybe n (_:xs) = listIndexMaybe (n-1) xs


mToS :: MiniLang -> State CFG S

mToS (MSlot n) =
    let i = n - 1
    in gets $ irOfObj . listIndexMaybe i . L.reverse . view stack

mToS (MBuiltin n) = return $ SVar $ Builtin n

mToS (MInt n) = return $ SInt n

mToS (MTuple elts) = STp <$> mapM mToS elts

mToS (MTerm n) = require n >>= \case
    Just v -> return $ irOfObj v
    _      -> return $ SVar (Lexical n)

mToS (MApp f args) = do
    f    <- mToS f
    args <- mapM mToS args
    return $ SCall f args

mToS (MCombine suite) = SCombine <$> mapM mToS suite
mToS (MAttr subject attr) = flip SAttr attr <$> mToS subject

analyse :: [P] -> State CFG Seman
analyse = \case
    [] -> do
        stack <- gets $ view stack
        let [SObj i] = stack
        return emptySeman { ret = i }
    
    PPred lang: xs -> do
        app <- mToS lang
        let prog = EPred app
        over route (prog :) <$> analyse xs
    
    (maybeShift -> Just x) : xs -> do
        obj <- shiftReduce
        push obj
        over route (x :) <$> analyse xs

    PBind s : xs -> do
        o <- peek0
        enter s o
        analyse xs


    PPushScope s : xs -> do
        pushScope s M.empty
        analyse xs

    PPopScope s : xs -> do
        (s', _) <- popScope
        when (s' /= s) $ error $ printf
            "expected popping (%s), got popping (%s)"
            s
            s'
        analyse xs

    PReduce m n : xs -> do
        app <- mToS m
        replicateM_ n pop
        obj <- newObj
        push obj
        let prog = EProc [refObj obj app]
        over route (prog :) <$> analyse xs

    PPack n : xs -> do
        tp  <- STp . reverse . map irOfObj <$> replicateM n pop
        obj <- newObj
        push obj
        let prog = EProc [refObj obj tp]
        over route (prog :) <$> analyse xs

    PMkSExp s n : xs -> do
        tp  <- STp . reverse . map irOfObj <$> replicateM n pop
        obj <- newObj
        push obj
        let sexp = SExp s tp
            prog = EProc [refObj obj sexp]
        over route (prog :) <$> analyse xs

pGToSG :: Grammar [P] -> Grammar Seman
pGToSG g =
    let transf lens =
            let f = map (flip evalState emptyCFG . analyse)
            in  M.map f $ view lens g
        prods' = transf prods
        leftR' = transf leftR
    in  Grammar prods' leftR'
