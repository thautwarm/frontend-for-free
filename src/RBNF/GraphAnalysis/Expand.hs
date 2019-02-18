{-# LANGUAGE LambdaCase #-}

module RBNF.GraphAnalysis.Expand where
import RBNF.Semantics
import RBNF.GraphAnalysis.IRs
import qualified Data.Map as M
import qualified Data.Set as S

expand :: ParserContext -> ExpandedGraph
expand ctx =
    fmap (expandEach S.empty) ctx
    where
        expandEach :: S.Set String -> Parser -> [ExpandedNodes]
        expandEach recur = \case
            RefP name ->
                -- inline to remove overhead of entering new function stacks without advancing tokenizers' offset.
                case M.lookup name ctx of
                    Just p | name `notElem` recur ->
                        expandEach (S.insert name recur) p
                    Just _ -> return . return $ RefE name
                    _ -> errorWithoutStackTrace $ "No parser named " ++ name ++ "."
            LitP lexer -> return . return $ LitE lexer 
            OrP p1 p2 -> expandEach recur p1 ++ expandEach recur p2
            AndP p1 p2 ->
                [ e1 ++ e2 | e1 <- expandEach recur p1, e2 <- expandEach recur p2 ]
            RepP p rng ->
                [ return $ repeatE e rng | e <- expandEach recur p ]
                where
                    repeatE ([a]) = Rep1E a
                    repeatE [] = errorWithoutStackTrace "Repeat an empty sequence of parsers."
                    repeatE xs = Rep2E xs