module RBNF.CodeGenIRs.ABuiltins
where

import RBNF.CodeGenIRs.A

tokenId = "idint"
tokenOff = "offset"
tokenCol = "colomn"
tokenLine = "lineno"
tokenVal = "val"
tokenName = "name" -- identical to Id

dsl_eq_n       = ABuiltin "=="
dsl_eq         = AVar dsl_eq_n

dsl_neq_n        = ABuiltin "!="
dsl_neq        = AVar dsl_neq_n

dsl_mv_forward_n = ABuiltin "mv_forward"
dsl_mv_forward = AVar dsl_mv_forward_n

dsl_peekable_n = ABuiltin "peekable"
dsl_peekable   = AVar dsl_peekable_n
dsl_peek_n     = ABuiltin "peek"
dsl_peek       = AVar dsl_peek_n
dsl_match_tk_n = ABuiltin "match_tk"
dsl_match_tk   = AVar dsl_match_tk_n
dsl_s_to_i_n   = ABuiltin "tk_id"
dsl_s_to_i     = AVar dsl_s_to_i_n

dsl_reset_n = ABuiltin "reset"
dsl_reset = AVar dsl_reset_n
dsl_cons_n = ABuiltin "cons"
dsl_cons = AVar dsl_cons_n
dsl_nil_n = ABuiltin "nil"
dsl_nil = AVar dsl_nil_n
dsl_null_n = ABuiltin "null"
dsl_null = AVar dsl_null_n
dsl_to_errs_n = ABuiltin "to_errs"
dsl_to_errs = AVar dsl_to_errs_n
dsl_to_res_n = ABuiltin "to_result"
dsl_to_res = AVar dsl_to_res_n
dsl_to_any_n = ABuiltin "to_any"
dsl_to_any = AVar dsl_to_any_n
dsl_mkast_n = ABuiltin "mk_ast"
dsl_mkast = AVar dsl_mkast_n

dsl_int :: Int -> AIR
dsl_int        = AInt . fromIntegral

dsl_true = ABool True
dsl_false = ABool False

dsl_tokens_n     = ABuiltin "tokens"
dsl_globST_n     = ABuiltin "state"
dsl_off_n        = ABuiltin "off"