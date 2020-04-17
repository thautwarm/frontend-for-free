module RBNF.IRs.MarisaLibrary where
import           RBNF.IRs.Marisa
import           RBNF.Name

tokenId = "idint"
tokenOff = "offset"
tokenCol = "colomn"
tokenLine = "lineno"
tokenVal = "val"
tokenName = "name" -- identical to Id

dsl_eq_n = Builtin "eq"
dsl_eq = MKVar dsl_eq_n

dsl_neq_n = Builtin "not_eq"
dsl_neq = MKVar dsl_neq_n

dsl_mv_forward_n = Builtin "mv_forward"
dsl_mv_forward = MKVar dsl_mv_forward_n

dsl_peekable_n = Builtin "peekable"
dsl_peekable = MKVar dsl_peekable_n
dsl_peek_n = Builtin "peek"
dsl_peek = MKVar dsl_peek_n
dsl_match_tk_n = Builtin "match_tk"
dsl_match_tk = MKVar dsl_match_tk_n

dsl_cons_n = Builtin "cons"
dsl_cons = MKVar dsl_cons_n
dsl_nil_n = Builtin "nil"
dsl_nil = MKVar dsl_nil_n
dsl_null_n = Builtin "null"
dsl_null = MKVar dsl_null_n
dsl_to_res_n = Builtin "to_result"
dsl_to_res = MKVar dsl_to_res_n
dsl_to_any_n = Builtin "to_any"
dsl_to_any = MKVar dsl_to_any_n
dsl_mkast_n = Builtin "mk_ast"
dsl_mkast = MKVar dsl_mkast_n

dsl_is_null_n = Builtin "is_null"
dsl_is_null = MKVar dsl_is_null_n

dsl_is_not_null_n = Builtin "is_not_null"
dsl_is_not_null = MKVar dsl_is_not_null_n


dsl_int :: Int -> Marisa
dsl_int = MKInt . fromIntegral

dsl_true = MKBool True
dsl_false = MKBool False

dsl_tokens_n = Builtin "tokens"
dsl_globST_n = Builtin "state"
