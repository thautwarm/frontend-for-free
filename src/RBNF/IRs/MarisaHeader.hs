-- | need some pre-built external declarations
module RBNF.IRs.MarisaHeader (addHeader) where
import           RBNF.Name
import           RBNF.HMTypeInfer
import           RBNF.TypeSystem
import           RBNF.IRs.MarisaLibrary
import           RBNF.IRs.Marisa
import           Control.Monad.State
import qualified Data.Set as S
tforall :: [String] -> HMT RT -> HMT RT
tforall = TForall . S.fromList

infixr 0 .:

a .:: b = (a, b)
(.:) :: _ -> _ -> State (Marisa -> Marisa) ()
a .: b = modify (. MKExtern a (Left b))

struct :: _ -> _ -> _ -> State (Marisa -> Marisa) _
struct name vars fields = do
    modify (. MKExtern (MName name) (Right (vars, fields)))
    return (TNom . RTSig $ MName name)

addHeader :: Bool -> Marisa -> Marisa
addHeader withTrace = flip execState id $ do
    let unit    = TNom $ RTPrim RTUnit
        int     = TNom $ RTPrim RTInt
        float   = TNom $ RTPrim RTFloat
        str     = TNom $ RTPrim RTString
        bool    = TNom $ RTPrim RTBool
        sup     = TNom $ RTPrim RTAny
        state   = TNom $ RTPrim RTState

    tokenT <- struct "token" [] [
            tokenId     .:: int
            , tokenCol  .:: int
            , tokenLine .:: int
            , tokenVal  .:: str
            , tokenName .:: str
        ]
    astT  <- struct "ast" [] []
    listT <- struct "linkedlist" ["a"] []
    tokensT <- struct "tokens" [] [
            tokenOff .:: int
        ]
    dsl_eq_n   .: tforall ["a"] $ (TFresh "a" :* TFresh "a") :-> bool
    dsl_neq_n  .: tforall ["a"] $ (TFresh "a" :* TFresh "a") :-> bool
    dsl_null_n .: tforall ["a"] $ TFresh "a"
    dsl_peekable_n .: (tokensT :* int) :-> bool
    dsl_peek_n     .:  (tokensT :* int) :-> tokenT

    let dsl_match_tk_ret_ty
            | withTrace = bool :* sup
            | otherwise = astT
    dsl_match_tk_n .: (tokensT :* int) :-> dsl_match_tk_ret_ty
    dsl_s_to_i_n   .: str :-> int
    dsl_reset_n    .: (tokensT :* int) :-> unit
    dsl_cons_n     .: tforall ["a"] $
                      (TFresh "a" :* listT :# TFresh "a") :->
                      listT :# TFresh "a"
    dsl_nil_n      .: tforall ["a"] $
                      listT :# TFresh "a"
    dsl_to_errs_n  .: sup :-> listT :# (int :* str)
    dsl_to_res_n   .: sup :-> astT
    dsl_to_any_n   .: tforall ["a"] $
                      TFresh "a" :-> sup
    dsl_mkast_n    .: tforall ["a"] $
                      (str :* TFresh "a") :-> astT
    dsl_is_null_n  .: tforall ["a"] $ TFresh "a" :-> bool
    MName "always_true" .: state :-> bool