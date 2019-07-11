{-# LANGUAGE LambdaCase #-}
module RBNF.GSS where

data GSS a
    = GSSCons (GSS a) a
    | GSSNil

toList :: GSS a -> [a]
toList = \case
    GSSCons tl hd -> hd:toList tl
    GSSNil        -> []

instance Show a => Show (GSS a) where
    show = show . toList

