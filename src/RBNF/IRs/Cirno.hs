module RBNF.IRs.Cirno
(Cirno(..))
where

import RBNF.Name (SVName)
import RBNF.Utils

data Cirno
    = CirnoAss SVName Cirno
    | CirnoTuple [Cirno]
    | CirnoVar SVName
    | CirnoMkSExp String Cirno
    | CirnoCall Cirno [Cirno]
    deriving (Eq, Ord)


instance Show Cirno where
  show ir = case ir of
      CirnoAss i c ->
          show i ++ " <- " ++ show c
      CirnoTuple xs ->
          "(" ++ intercalate "," (map show xs) ++ ")"
      CirnoVar n -> show n
      CirnoMkSExp n c -> n ++ "{" ++ show c ++ "}"
      CirnoCall f args ->
          let args_str = "(" ++ intercalate "," (map show args) ++ ")"
          in show f ++ args_str