module RBNF.CodeGenIRs.ABuiltins
where

import RBNF.CodeGenIRs.A
dsl_eq         = AVar $ ABuiltin "=="
dsl_neq        = AVar $ ABuiltin "!="
dsl_mv_forward = AVar $ ABuiltin "mv_forward"
dsl_peekable_n = AVar $ ABuiltin "peekable_n"
dsl_peek_n     = AVar $ ABuiltin "peek_n"
dsl_match_tk   = AVar $ ABuiltin "match_tk"
dsl_s_to_i     = AVar $ ABuiltin "tk_id"

dsl_reset      = AVar $ ABuiltin "reset"
dsl_cons       = AVar $ ABuiltin "cons"
dsl_nil        = AVar $ ABuiltin "nil"
dsl_null       = AVar $ ABuiltin "null"
dsl_to_errs    = AVar $ ABuiltin "to_errs"
dsl_to_res     = AVar $ ABuiltin "to_result"
dsl_to_any     = AVar $ ABuiltin "to_any"
dsl_mkast      = AVar $ ABuiltin "mk_ast"

dsl_int :: Int -> AIR
dsl_int        = AInt . fromIntegral

dsl_tokens_n     = ABuiltin "tokens"
dsl_globST_n     = ABuiltin "state"
dsl_off_n        = ABuiltin "off"