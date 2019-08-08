module RBNF.TypeSystem where
import RBNF.Utils
import RBNF.Name

import GHC.Generics (Generic)
data RTPrim
    = RTInt
    | RTFloat
    | RTUnit
    | RTString
    | RTAny
    | RTBool
    -- blackboxed builtins
    | RTState
    deriving (Eq, Ord, Generic)

instance Show RTPrim where
    show = \case
        RTInt -> "int"
        RTFloat -> "float"
        RTUnit -> "()"
        RTString -> "str"
        RTAny -> "any"
        RTBool -> "bool"
        RTState -> "State"

-- RT for Reimu type, not for runtime
data RT
    = RTPrim RTPrim
    | RTTuple [RT]
    | RTFunc RT RT
    | RTApp RT RT
    -- we just compare structures by its names(and type parameters).
    -- our repo is for parsing instead of a general programming language...
    | RTSig MName
    | RTVar String
    | RTGeneric [String] RT
    deriving (Eq, Ord, Generic)

instance Show RT where
    show = \case
        RTVar s -> s
        RTPrim s    -> show s
        RTFunc a b  -> showNest a ++ " -> " ++ show b
        RTTuple xs  -> "(" ++ intercalate "," (map show xs) ++ ")"
        RTGeneric xs t -> "forall " ++ (unwords xs) ++ ". " ++ show t
        RTApp t1 t2    -> show t1 ++ " " ++ showNest t2
        RTSig s        -> show s
        where
            showNest s
                | isNest s  = "(" ++ show s ++ ")"
                | otherwise = show s
            isNest s = case s of
                RTFunc _ _    -> True
                RTGeneric _ _ -> True
                _             -> False