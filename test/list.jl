function rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_tmp_1 =  builtin_match_tk(builtin_tokens, builtin_tk_id("quote a"))
  if builtin_is_null(rbnf_tmp_1)
      builtin_null
  else
      rbnf_tmp_1_ =  append(rbnf_tmp_0, rbnf_tmp_1)
      rbnf_tmp_1_
  end
end
function rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_0_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_0_try =  rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_is_not_null(rbnf_named_lr_rbnfmacro_0_try)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_0_reduce =  rbnf_named_lr_rbnfmacro_0_try
      rbnf_named_lr_rbnfmacro_0_try =  rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_0_reduce),)
  else
      rbnf_named_lr_rbnfmacro_0_try
  end
end
function rbnf_named_parse_START(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_g(builtin_state, builtin_tokens)
  if builtin_is_null(rbnf_named__check_0)
      builtin_null
  else
      rbnf_tmp_0 =  rbnf_named__check_0
      rbnf_tmp_1_ =  builtin_mk_ast("START", ((rbnf_tmp_0),))
      rbnf_tmp_1_
  end
end
function rbnf_named_parse_g(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                         builtin_tokens)
  if builtin_is_null(rbnf_named__check_0)
      builtin_null
  else
      rbnf_tmp_0 =  rbnf_named__check_0
      rbnf_tmp_1_ =  builtin_mk_ast("g", ((rbnf_tmp_0),))
      rbnf_tmp_1_
  end
end
function rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, builtin_tk_id("quote a"))
  if builtin_is_null(rbnf_tmp_0)
      builtin_null
  else
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list, rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end