module RBNF.IRs.MarisaHeader where
import           RBNF.Name
import           RBNF.HMTypeInfer
import           RBNF.TypeSystem
import           RBNF.IRs.MarisaLibrary
import           RBNF.IRs.Marisa
import qualified Data.List as S
import           Control.Monad.State

tforall :: [String] -> T -> T
tforall = TForall . S.fromList

infixr 0 <:

a .:: b = (a, b)
a .: b = modify $ MExtern a (Left b)
struct name vars fields = do
    modify $ MExtern (MName name) (Right (vars, fields))
    return (TNom . RTSig $ MName name)

addHeader :: State (Marisa -> Marisa) State
addHeader withTrace marisa = do
    let unit    = TNom $ RTPrim RTUnit
        int     = TNom $ RTPrim RTInt
        float   = TNom $ RTPrim RTFloat
        str     = TNom $ RTPrim RTString
        bool    = TNom $ RTPrim RTBool
        sup     = TNom $ RTPrim RTAny
        state   = TNom $ RTPrim RTState
        tokens  = TNom $ RTPrim RTTokens

    tokenT <- struct "token" [] [
            tokenId     .:: int
            , tokenCol  .:: int
            , tokenLine .:: int
            , tokenVal  .:: str
            , tokenName .:: str
        ]
    astT  <- struct "ast" [] []
    listT <- struct "linkedlist" ["a"]
    tokensT <- struct "tokens" [] [
            tokenOff .:: int
        ]
    dsl_eq_n   .: tforall ["a"] $ (TFresh "a" :* TFresh "a") -> bool
    dsl_neq_n  .: tforall ["a"] $ (TFresh "a" :* TFresh "a") -> bool
    dsl_null_n .: tforall ["a"] $ TFresh "a"
    
        ast     = TNom (RTSig "ast")    :$ unit
        tokenTy = TNom (RTSig "token") :# unit
        tokensTy = TNom (RTSig "tokens") :# unit
        listT   = TNom $ RTSig "linkedlist"
        null    = tforall ["a"] $ TFresh "a"
        traceTy = int :* str
        cmpTy   = tforall ["a"] $ (TFresh "a" :* TFresh "a") :-> bool
    in MKExtern (MName "token") (Right ([], [
            (tokenId,   int),
            (tokenCol,  int),
            (tokenLine, int),
            (tokenVal,  str),
            (tokenName, str)
        ]))                                               $
        MKExtern (MName "token") (Right ([], []))         $
        MKExtern (MName "linkedlist") (Right (["a"], [])) $
        MKExtern (MName "tokens") (Right ([], [
                (tokenOff, int)                  ]))      $
        