{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import RBNF (parserGen)
import RBNF.Serialization
import RBNF.IRs.Marisa
import System.IO
import System.Environment
import System.Exit
import Control.Lens

import Data.Text.Prettyprint.Doc.Util
import qualified Data.Text.Lazy.IO as T
import qualified Data.Map as M

data Flag
    = Aeson String
    | Dump

main = getArgs >>= wain

parseOptsKey m = \case
    "-h":xs     -> parseOptsKey (M.insert "h" "" m) xs
    "-aeson":xs -> parseOptVal "aeson" m xs
    "-dump":xs  -> parseOptsKey (M.insert "dump" "" m) xs
    "-k":xs     -> parseOptVal "k" m xs
    "-trace":xs -> parseOptVal "trace" m xs
    "-in":xs -> parseOptVal "in" m xs
    k:xs        -> Left k
    []          -> Right m

parseOptVal k m = \case
        v:xs    -> parseOptsKey (M.insert k v m) xs
        []      -> Left k

wain xs =
    case parseOptsKey M.empty xs of
    Left k  -> putStrLn $ "Error in key " ++ k ++ "."
    Right m ->
        case M.lookup "v" m of
            Just _ -> version >> exit
            Nothing -> m1
        where
            help = usage >> exit
            k :: Maybe Int
            k  = fmap read $ M.lookup "k" m
            trace :: Bool
            trace  = "trace" `M.member` m
            m1 :: IO ()
            m1 = case M.lookup "h" m of
                    Just _  -> help
                    Nothing -> m2
            m2 = case M.lookup "v" m of
                    Just _   -> version >> exit
                    Nothing  -> m3

            m3 = case M.lookup "in" m of
                    Just in' -> m4 in'
                    Nothing  -> help

            m4 in' = case M.lookup "aeson" m of
                    Just out -> do
                        text <- T.readFile in'
                        mio (T.writeFile out . dumpAIR) text
                    _ -> m5 in'
            m5 in' = case M.lookup "dump" m of
                    Just _ -> do
                        text <- T.readFile in'
                        mio (putDocW  80 . seeMarisa) text
                    _ -> help
            mio f text = case readCG text of
                Just cg ->
                    case k of
                        Just k -> f $ parserGen k trace cg
                        Nothing -> putStrLn "Lookahead K not specified."
                _ -> putStrLn "JSON cannot get parsed as CGrammar."


usage   = putStrLn "Usage: [-v] [-h] [-in input-filename] [-dump] file"
version = putStrLn "2.0"
exit    = exitSuccess