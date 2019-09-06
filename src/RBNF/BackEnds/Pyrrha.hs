-- | The Python back end
{-# LANGUAGE OverloadedStrings #-}
module RBNF.BackEnds.Pyrrha where

import           RBNF.IRs.Marisa
import           RBNF.Name
import           RBNF.Utils
import           Control.Monad.State
import           Codec.Binary.UTF8.String      as UTF8

import qualified Data.Text                     as T
import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map                      as M

showVar :: MName -> Text
showVar = mangleOC . pack . show

mangleOC :: Text -> Text
mangleOC = mangling "." "_" . mangling "%" "prim_" . mangling "-" "local_"


type PyName = Text
gensym :: Int -> PyName
gensym i = T.pack $ "lcl_" ++ show i

data CFG
    = CFG {
        _lcls    :: S.Set PyName
    ,   _rus     :: S.Set PyName
    ,   _lyt     :: Int
    ,   _cde     :: [(Int, Text)]
    }
emptyCFG = CFG S.empty S.empty 0 []

makeLenses ''CFG

py_build :: Text -> State CFG ()
py_build a = do
    i <- gets $ view lyt
    modify $ over cde ((i, a) :)
py_release :: PyName -> State CFG ()
py_release n = do
    locals <- gets $ view lcls
    when (n `S.member` locals) $ do
        modify $ over lcls (S.delete n)
        modify $ over rus (S.insert n)

py_alloc :: State CFG PyName
py_alloc = do
    reusable <- gets $ view rus
    if S.null reusable then do
        locals    <- gets $ view lcls
        let new_tmp = gensym (S.size locals)
        modify $ over lcls (S.insert new_tmp)
        return new_tmp
    else
        let (hd, tl) = S.splitAt 1 reusable in
        do modify $ over rus (const tl)
           return $ S.elemAt 0 hd

py_indent :: State CFG ()
py_indent = modify $ over lyt (+1)

py_dedent :: State CFG ()
py_dedent = modify $ over lyt (\x -> x - 1)

py_assign :: PyName -> [Text] -> State CFG ()
py_assign lhs rhs = py_build $ T.concat (lhs : " = " : rhs)

within_py_indent :: State CFG a -> State CFG a
within_py_indent m = do
    py_indent
    r <- m
    py_dedent
    return r

within_py_scope :: State CFG a -> State CFG a
within_py_scope m = do
    locals   <- gets $ view lcls
    reusable <- gets $ view rus
    r <- m
    modify $ over lcls (const locals)
    modify $ over rus (const reusable)
    return r

cgPy :: Marisa -> State CFG Text
cgPy = \case
    MKAssign name ir -> do
        let lhs = showVar name
        rhs <- cgPy ir
        py_release rhs
        py_build $ T.concat [lhs, " = ", rhs]
        return lhs
    MKCall f args -> do
        f <- cgPy f
        args <- mapM cgPy args
        py_release f
        forM_ args py_release
        lhs <- py_alloc
        py_assign lhs [f, "(", T.intercalate ", " args, ")"]
        return lhs
    MKAttr v attr -> do
        v <- cgPy v
        py_release v
        lhs <- py_alloc
        py_assign lhs [v, ".", T.pack attr]
        return lhs
    MKPrj v i -> do
        v <- cgPy v
        py_release v
        lhs <- py_alloc
        py_assign lhs [v, "[", T.pack $ show i, "]"]
        return lhs
    MKIf cond trueC falseC -> do
        cond <- cgPy cond
        py_release cond
        lhs  <- py_alloc
        py_build $ T.concat ["if ", cond, ":"]
        within_py_indent $ do
            trueC <- cgPy trueC
            py_release trueC
            py_assign lhs [trueC]

        py_build $ "else:"
        within_py_indent $ do
            falseC <- cgPy falseC
            py_release falseC
            py_assign lhs [falseC]
        return lhs
    MKWhile cond body -> do
        cond <- cgPy cond
        py_release cond
        py_build $ T.concat ["while ", cond, ":"]
        within_py_indent $ do
            body <- cgPy body
            py_release body
        return "None"
    MKSwitch test [] defaultCase -> do
        test <- cgPy test
        py_release test
        cgPy defaultCase
    MKSwitch test (hd:tl) defaultCase -> do
        lhs <- py_alloc
        test <- cgPy test
        let genif token (c, e) = do
                c <- cgPy c
                py_build $ T.concat [token, test, " = ", c, ":"]
                within_py_indent $ do
                    e <- cgPy e
                    py_release e
                    py_assign lhs [e]
        genif "if " hd
        forM_ tl $ genif "elseif "
        py_build $ "else:"
        within_py_indent $ do
            defaultCase <- cgPy defaultCase
            py_release defaultCase
            py_assign lhs [defaultCase]
        py_release test
        return lhs
    MKDef fname args expr -> do
        fname <- return $ showVar fname
        args  <- return $ map showVar args
        py_build $ T.concat ["def ", fname, "(", T.intercalate ", " args , ")", ":"]
        within_py_indent . within_py_scope $ do
            expr <- cgPy expr
            py_build $ T.concat ["return ", expr]
        return fname
    MKBlock [] -> return "None"
    MKBlock suite -> do
        let hd: tl' = reverse suite
            tl      = reverse suite
        forM_ tl $ \x ->
            py_release <$> cgPy x
        cgPy hd
    MKVar n -> return $ showVar n
    MKInt n -> return $ pack (show n)
    MKStr n -> return $ pack (show n)
    MKBool True -> return "True"
    MKBool False -> return "False"
    MKTuple elts -> do
        elts <- mapM cgPy elts
        forM_ elts py_release
        lhs <- py_alloc
        let el = case elts of
                [x] -> ","
                _   -> ""
        py_assign lhs ["(", T.intercalate ", " elts, el, ")"]
        return lhs
    MKAnd a b -> do
        a <- cgPy a
        b <- cgPy b
        py_release a
        py_release b
        lhs <- py_alloc
        py_assign lhs [a, " and ", b]
        return lhs
    MKOr a b -> do
        a <- cgPy a
        b <- cgPy b
        py_release a
        py_release b
        lhs <- py_alloc
        py_assign lhs [a, " or ", b]
        return lhs
    MKExtern _ _ body -> cgPy body

mk_indent :: Int -> Text
mk_indent = flip T.replicate $ "    "

pyGen :: Marisa -> Text
pyGen marisa = T.unlines [T.concat [mk_indent i, c] | (i, c) <- reverse codes]
    where codes = view cde $ execState (cgPy marisa) emptyCFG