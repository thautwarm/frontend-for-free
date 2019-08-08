module RBNF.IRs.MarisaLibrary where
import           RBNF.IRs.Marisa
import           RBNF.Name

tokenId = "idint"
tokenOff = "offset"
tokenCol = "colomn"
tokenLine = "lineno"
tokenVal = "val"
tokenName = "name" -- identical to Id

dsl_eq_n = MBuiltin "=="
dsl_eq = MKVar dsl_eq_n

dsl_neq_n = MBuiltin "!="
dsl_neq = MKVar dsl_neq_n

dsl_mv_forward_n = MBuiltin "mv_forward"
dsl_mv_forward = MKVar dsl_mv_forward_n

dsl_peekable_n = MBuiltin "peekable"
dsl_peekable = MKVar dsl_peekable_n
dsl_peek_n = MBuiltin "peek"
dsl_peek = MKVar dsl_peek_n
dsl_match_tk_n = MBuiltin "match_tk"
dsl_match_tk = MKVar dsl_match_tk_n
dsl_s_to_i_n = MBuiltin "tk_id"
dsl_s_to_i = MKVar dsl_s_to_i_n

dsl_reset_n = MBuiltin "reset"
dsl_reset = MKVar dsl_reset_n
dsl_cons_n = MBuiltin "cons"
dsl_cons = MKVar dsl_cons_n
dsl_nil_n = MBuiltin "nil"
dsl_nil = MKVar dsl_nil_n
dsl_null_n = MBuiltin "null"
dsl_null = MKVar dsl_null_n
dsl_to_errs_n = MBuiltin "to_errs"
dsl_to_errs = MKVar dsl_to_errs_n
dsl_to_res_n = MBuiltin "to_result"
dsl_to_res = MKVar dsl_to_res_n
dsl_to_any_n = MBuiltin "to_any"
dsl_to_any = MKVar dsl_to_any_n
dsl_mkast_n = MBuiltin "mk_ast"
dsl_mkast = MKVar dsl_mkast_n

dsl_int :: Int -> Marisa
dsl_int = MKInt . fromIntegral

dsl_true = MKBool True
dsl_false = MKBool False

dsl_tokens_n = MBuiltin "tokens"
dsl_globST_n = MBuiltin "state"
