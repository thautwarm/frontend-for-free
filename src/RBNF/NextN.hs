{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module RBNF.NextN where

import RBNF.Symbols

data NextN
    = NextN of [(Case, NextN)]

data CSGProgram
    = CSGPack Int
    | CSGReduce MiniLang Int
    | CSGPred MiniLang
    | CSGBind String
    | CSGModif MiniLang

data CSGS
    = CSGTerm Case
    | CSGNonTerm String
    | CSGProgram
