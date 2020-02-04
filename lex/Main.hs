module Main where
import           System.IO
import           System.Environment
import           System.Exit
import           Control.Monad
import           RBNF.FrontEnd
import           RBNF.Constructs (C(..), CProd)
import qualified Data.Map                      as M
import qualified Data.List                     as L

main = getArgs >>= wain

parseOptsKey m = \case
    "-h"   : xs -> parseOptsKey (M.insert "h" "" m) xs
    "-v"   : xs -> parseOptsKey (M.insert "v" "" m) xs
    "-out" : xs -> parseOptVal "out" m xs
    "-in"  : xs -> parseOptVal "in" m xs
    k      : xs -> Left k
    []          -> Right m

parseOptVal k m = \case
    v : xs -> parseOptsKey (M.insert k v m) xs
    []     -> Left k


terminals :: [CProd] -> [String]
terminals g = L.nub $ productions >>= terminalsOf
    where
        productions = flip map g $ \case (_, c, _) -> c
        terminalsOf :: C -> [String]
        terminalsOf = \case
            CTerm s    -> return s
            CSeq xs    -> xs >>= terminalsOf
            CAlt xs    -> xs >>= terminalsOf
            COpt c     -> terminalsOf c
            CBind n c  -> terminalsOf c
            _          -> mzero

wain xs = case parseOptsKey M.empty xs of
    Left  k -> putStrLn $ "Error in key " ++ k ++ "."
    Right m -> do
        may_print_help
        may_print_ver
        outfn <- outfn
        inStr <- inStr
        combinatoric <- case parseDoc inStr of
            Left err     -> putStrLn err >> exitFailure
            Right (a, s) -> return a
        outfn (unlines . terminals $  combinatoric)
      where
        may_print_help :: IO ()
        may_print_help | "h" `M.member` m = help >> exitSuccess
                       | otherwise        = return ()
        may_print_ver :: IO ()
        may_print_ver | "v" `M.member` m = version >> exitSuccess
                      | otherwise        = return ()
        inStr :: IO String
        inStr = case M.lookup "in" m of
            Just fname -> readFile fname
            _          -> putStrLn "Input file not specified." >> exitFailure

        outf :: IO String
        outf = case M.lookup "out" m of
            Just a -> return a
            _      -> putStrLn "Output file not specified." >> exitFailure

        outfn :: IO (String -> IO ())
        outfn = do
            outf <- outf
            let apply
                  | outf == "stdout" = putStrLn
                  | otherwise        = writeFile outf
            return apply
version = putStrLn "2.0"
help = putStrLn "Usage: [-v] [-h] [-in filename] [-out filename]\n"
