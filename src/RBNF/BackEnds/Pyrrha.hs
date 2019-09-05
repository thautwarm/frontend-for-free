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

py_assign lhs rhs = py_build $ T.concat (lhs : " = " : rhs)

cgPy :: Marisa -> State CFG Text
cgPy = \case
    MKAssign name ir ->
        let lhs = showVar name in do
                rhs <- cgPy ir
                py_build $ T.concat [lhs, " = ", rhs]
                py_release rhs
                return lhs
    MKCall f args -> do
        f <- cgPy f
        args <- mapM cgPy args
        lhs <- py_alloc
        py_assign lhs [f, "(", T.intercalate ", " args, ")"]
        return lhs
    MKAttr v attr -> do
        v <- cgPy v
        lhs <- py_alloc
        py_assign lhs [v, ".", T.pack attr]
        return lhs
    MKPrj v i -> do
        v <- cgPy v
        lhs <- py_alloc
        py_assign lhs [v, "[", T.pack $ show i, "]"]
        return lhs
    MKIf cond trueC falseC -> do
        cond <- cgPy cond
        lhs  <- py_alloc
        py_build $ T.concat ["if ", cond, ":"]
        py_indent
        trueC <- cgPy trueC
        py_assign lhs [trueC]
        py_dedent
        py_build $ "else:"
        py_indent
        falseC <- cgPy falseC
        py_assign lhs [falseC]
        py_dedent
        return lhs
    MKWhile cond body -> do
        cond <- cgPy cond
        py_build $ T.concat ["while ", cond, ":"]
        py_indent
        body <- cgPy body
        lhs <- py_alloc
        py_assign lhs [body]
        py_dedent
        return lhs