-- | This module provides name representations for various IRs.
module RBNF.Name where
import GHC.Generics

-- | SVName is for IRs on a stack based parsing machine.
--   SVSlot i represents the parsing result of i-th symbol in a parsing rule.
data SVName
    = SVLocal String
    | SVSlot  Int
    deriving (Eq, Ord)


instance Show SVName where
    show = \case
        SVSlot i -> "slots["++ show i ++"]"
        SVLocal s -> s

-- | Beside the normal name(by constructor 'MName'),
--  the type MName provides a sort of names that can be
--  distinguished from user defined names(by constructor `MBuiltin`)
data MName = MName String | MBuiltin String
    deriving (Eq, Ord, Generic)

instance Show MName where
    show = \case
        MName s -> s
        MBuiltin s -> "%" ++ s