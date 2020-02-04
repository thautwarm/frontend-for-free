module RBNF.Constructs where
import           RBNF.Utils

import           GHC.Generics                   ( Generic )

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.List                     as L
import           Control.Monad.State
import           Control.Arrow

data MiniLang
    = MTerm String
    | MBuiltin String
    | MSlot Int
    | MInt Int
    | MTuple [MiniLang]
    | MApp MiniLang [MiniLang]
    deriving (Eq, Ord, Generic, Read)

-- Parsing Constructs
-- Also can be regarded as instructions
-- of a stack-based VM
data P
    = PTerm String
    -- PNonTerm symbol is_grammar_node
    | PNonTerm String
    -- advanced:
    | PBind String

    -- PPack TopN_of_stack Reduce_func
    | PPack   Int
    | PReduce MiniLang Int
    | PMkSExp String Int

    | PPred MiniLang

    | PPushScope String -- String for debugging
    | PPopScope  String -- String for debugging
    deriving (Eq, Ord)

type PProd = (String, [P])


-- Combinatorial
data C
    = CTerm    String
    | CNonTerm String
    | CSeq     [C]
    | CAlt     [C]
    | COpt     C
    | CPred    MiniLang
    -- advanced:
    | CBind    String C
    deriving (Eq, Ord, Generic, Read)

type CProd = (String, C, Maybe MiniLang)

-- Combinatorial

maybeShiftTerm :: P -> Bool
maybeShiftTerm = \case
    PTerm    _ -> True
    PNonTerm _ -> True
    _          -> False

instance Show C where
    show = \case
        CTerm    c   -> "<" ++ c ++ ">"
        CNonTerm s   -> s
        CSeq     [c] -> show c
        CSeq     cs  -> "(" ++ unwords (map show cs) ++ ")"
        CAlt     [c] -> show c
        CAlt     cs  -> "(" ++ L.intercalate "|" (map show cs) ++ ")"
        COpt     c   -> "[" ++ show c ++ "]"
        CBind name c -> name ++ "=" ++ show c

instance Show P where
    show = \case
        PTerm    c    -> "<" ++ c ++ ">"
        PNonTerm s    -> s
        PPack    n    -> "pack<" ++ show n ++ ">"
        PReduce app n -> "reduce<" ++ show n ++ ", " ++ show app ++ ">"
        PBind s       -> "=: " ++ s
        PMkSExp tag n -> "s-exp<" ++ tag ++ ", " ++ show n ++ ">"
        PPushScope s  -> "pushscope " ++ s
        PPopScope  s  -> "popscope " ++ s

instance Show MiniLang where
    show = \case
        MTerm s -> s
        MApp f args ->
            show f ++ "(" ++ L.intercalate ", " (map show args) ++ ")"
