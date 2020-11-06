function rbnf_named_lr_step_expression(rbnf_tmp_0,
                                       builtin_state,
                                       builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 53
        rbnf_named__check_1 =  rbnf_named_parse_DIVIDE(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_2[0], False)
                rbnf_named__check_2
            else
                rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                rbnf_tmp_1_ =  builtin_mk_ast("expression",
                               ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 3
        rbnf_named__check_1 =  rbnf_named_parse_MINUS(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_2[0], False)
                rbnf_named__check_2
            else
                rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                rbnf_tmp_1_ =  builtin_mk_ast("expression",
                               ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 2
        rbnf_named__check_1 =  rbnf_named_parse_PLUS(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_2[0], False)
                rbnf_named__check_2
            else
                rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                rbnf_tmp_1_ =  builtin_mk_ast("expression",
                               ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 52
        rbnf_named__check_1 =  rbnf_named_parse_TIMES(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_2[0], False)
                rbnf_named__check_2
            else
                rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                rbnf_tmp_1_ =  builtin_mk_ast("expression",
                               ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "expression lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "expression got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_lr_loop_expression(rbnf_tmp_0,
                                       builtin_state,
                                       builtin_tokens)
  rbnf_named_lr_expression_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_expression_try =  rbnf_named_lr_step_expression(rbnf_named_lr_expression_reduce,
                                  builtin_state,
                                  builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_expression_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_expression_reduce =  builtin_to_result(rbnf_named_lr_expression_try[1])
      rbnf_named_lr_expression_try =  rbnf_named_lr_step_expression(rbnf_named_lr_expression_reduce,
                                      builtin_state,
                                      builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_expression_reduce),)
  else
      rbnf_named_lr_expression_try
  end
end
function rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_named__check_2 =  rbnf_named_parse_allInstr(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_2[0], False)
          rbnf_named__check_2
      else
          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
          rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_2)
          ((True, rbnf_tmp_1_),)
      end
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
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_0_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_0_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_0_try[1])
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
function rbnf_named_lr_step_rbnfmacro_1(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_modifier(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_1_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_1_try =  rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_1_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_1_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_1_try[1])
      rbnf_named_lr_rbnfmacro_1_try =  rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_1_reduce),)
  else
      rbnf_named_lr_rbnfmacro_1_try
  end
end
function rbnf_named_lr_step_rbnfmacro_10(rbnf_tmp_0,
                                         builtin_state,
                                         builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_pragma_name(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_lr_loop_rbnfmacro_10(rbnf_tmp_0,
                                         builtin_state,
                                         builtin_tokens)
  rbnf_named_lr_rbnfmacro_10_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_10_try =  rbnf_named_lr_step_rbnfmacro_10(rbnf_named_lr_rbnfmacro_10_reduce,
                                    builtin_state,
                                    builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_10_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_10_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_10_try[1])
      rbnf_named_lr_rbnfmacro_10_try =  rbnf_named_lr_step_rbnfmacro_10(rbnf_named_lr_rbnfmacro_10_reduce,
                                        builtin_state,
                                        builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_10_reduce),)
  else
      rbnf_named_lr_rbnfmacro_10_try
  end
end
function rbnf_named_lr_step_rbnfmacro_2(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_named__check_2 =  rbnf_named_parse_param(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_2[0], False)
          rbnf_named__check_2
      else
          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
          rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_lr_loop_rbnfmacro_2(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_2_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_2_try =  rbnf_named_lr_step_rbnfmacro_2(rbnf_named_lr_rbnfmacro_2_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_2_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_2_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_2_try[1])
      rbnf_named_lr_rbnfmacro_2_try =  rbnf_named_lr_step_rbnfmacro_2(rbnf_named_lr_rbnfmacro_2_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_2_reduce),)
  else
      rbnf_named_lr_rbnfmacro_2_try
  end
end
function rbnf_named_lr_step_rbnfmacro_3(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_qubit(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_lr_loop_rbnfmacro_3(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_3_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_3_try =  rbnf_named_lr_step_rbnfmacro_3(rbnf_named_lr_rbnfmacro_3_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_3_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_3_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_3_try[1])
      rbnf_named_lr_rbnfmacro_3_try =  rbnf_named_lr_step_rbnfmacro_3(rbnf_named_lr_rbnfmacro_3_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_3_reduce),)
  else
      rbnf_named_lr_rbnfmacro_3_try
  end
end
function rbnf_named_lr_step_rbnfmacro_4(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_named__check_2 =  rbnf_named_parse_variable(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_2[0], False)
          rbnf_named__check_2
      else
          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
          rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_lr_loop_rbnfmacro_4(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_4_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_4_try =  rbnf_named_lr_step_rbnfmacro_4(rbnf_named_lr_rbnfmacro_4_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_4_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_4_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_4_try[1])
      rbnf_named_lr_rbnfmacro_4_try =  rbnf_named_lr_step_rbnfmacro_4(rbnf_named_lr_rbnfmacro_4_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_4_reduce),)
  else
      rbnf_named_lr_rbnfmacro_4_try
  end
end
function rbnf_named_lr_step_rbnfmacro_5(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_named__check_2 =  rbnf_named_parse_matrixRow(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_2[0], False)
          rbnf_named__check_2
      else
          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
          rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_2)
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_lr_loop_rbnfmacro_5(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_5_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_5_try =  rbnf_named_lr_step_rbnfmacro_5(rbnf_named_lr_rbnfmacro_5_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_5_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_5_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_5_try[1])
      rbnf_named_lr_rbnfmacro_5_try =  rbnf_named_lr_step_rbnfmacro_5(rbnf_named_lr_rbnfmacro_5_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_5_reduce),)
  else
      rbnf_named_lr_rbnfmacro_5_try
  end
end
function rbnf_named_lr_step_rbnfmacro_6(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_2[0], False)
          rbnf_named__check_2
      else
          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
          rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_lr_loop_rbnfmacro_6(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_6_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_6_try =  rbnf_named_lr_step_rbnfmacro_6(rbnf_named_lr_rbnfmacro_6_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_6_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_6_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_6_try[1])
      rbnf_named_lr_rbnfmacro_6_try =  rbnf_named_lr_step_rbnfmacro_6(rbnf_named_lr_rbnfmacro_6_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_6_reduce),)
  else
      rbnf_named_lr_rbnfmacro_6_try
  end
end
function rbnf_named_lr_step_rbnfmacro_7(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_qubitVariable(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_lr_loop_rbnfmacro_7(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_7_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_7_try =  rbnf_named_lr_step_rbnfmacro_7(rbnf_named_lr_rbnfmacro_7_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_7_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_7_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_7_try[1])
      rbnf_named_lr_rbnfmacro_7_try =  rbnf_named_lr_step_rbnfmacro_7(rbnf_named_lr_rbnfmacro_7_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_7_reduce),)
  else
      rbnf_named_lr_rbnfmacro_7_try
  end
end
function rbnf_named_lr_step_rbnfmacro_8(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_named__check_2 =  rbnf_named_parse_instr(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_2[0], False)
          rbnf_named__check_2
      else
          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
          rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_2)
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_8_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_8_try =  rbnf_named_lr_step_rbnfmacro_8(rbnf_named_lr_rbnfmacro_8_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_8_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_8_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_8_try[1])
      rbnf_named_lr_rbnfmacro_8_try =  rbnf_named_lr_step_rbnfmacro_8(rbnf_named_lr_rbnfmacro_8_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_8_reduce),)
  else
      rbnf_named_lr_rbnfmacro_8_try
  end
end
function rbnf_named_lr_step_rbnfmacro_9(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named__check_1 =  rbnf_named_parse_offsetDescriptor(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_1[0], False)
      rbnf_named__check_1
  else
      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
      rbnf_tmp_1_ =  array_push(rbnf_tmp_0, rbnf_tmp_1)
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_lr_loop_rbnfmacro_9(rbnf_tmp_0,
                                        builtin_state,
                                        builtin_tokens)
  rbnf_named_lr_rbnfmacro_9_reduce =  rbnf_tmp_0
  rbnf_named__off_0 =  builtin_tokens.offset
  rbnf_named_lr_rbnfmacro_9_try =  rbnf_named_lr_step_rbnfmacro_9(rbnf_named_lr_rbnfmacro_9_reduce,
                                   builtin_state,
                                   builtin_tokens)
  while builtin_not_eq(rbnf_named_lr_rbnfmacro_9_try[0], False)
      rbnf_named__off_0 =  builtin_tokens.offset
      rbnf_named_lr_rbnfmacro_9_reduce =  builtin_to_result(rbnf_named_lr_rbnfmacro_9_try[1])
      rbnf_named_lr_rbnfmacro_9_try =  rbnf_named_lr_step_rbnfmacro_9(rbnf_named_lr_rbnfmacro_9_reduce,
                                       builtin_state,
                                       builtin_tokens)
  end
  if builtin_eq(builtin_tokens.offset, rbnf_named__off_0)
      ((True, rbnf_named_lr_rbnfmacro_9_reduce),)
  else
      rbnf_named_lr_rbnfmacro_9_try
  end
end
function rbnf_named_parse_ADD(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 29)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote ADD not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("ADD", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_AND(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 25)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote AND not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("AND", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_AS(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 4)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote AS not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("AS", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_AT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 67)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote @ not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("AT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_CIS(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 49)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote CIS not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("CIS", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_COLON(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 65)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote : not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("COLON", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 62)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote , not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("COMMA", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_CONTROLLED(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 55)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote CONTROLLED not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("CONTROLLED", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_CONVERT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 35)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote CONVERT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("CONVERT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_COS(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 46)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote COS not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("COS", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_DAGGER(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 56)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote DAGGER not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("DAGGER", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_DECLARE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 18)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote DECLARE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("DECLARE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_DEFCIRCUIT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 6)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote DEFCIRCUIT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("DEFCIRCUIT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_DEFGATE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 5)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote DEFGATE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("DEFGATE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_DIV(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 32)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote DIV not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("DIV", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_DIVIDE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 53)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote / not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("DIVIDE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_EQ(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 36)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote EQ not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("EQ", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_EXCHANGE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 34)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote EXCHANGE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("EXCHANGE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_EXP(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 48)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote EXP not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("EXP", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_FALSE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 24)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote FALSE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("FALSE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_FLOAT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 59)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "FLOAT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("FLOAT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_GE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 38)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote GE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("GE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_GT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 37)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote GT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("GT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_HALT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 9)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote HALT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("HALT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_I(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 44)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote i not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("I", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_IDENTIFIER(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 57)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "IDENTIFIER not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("IDENTIFIER", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_INCLUDE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 16)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote INCLUDE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("INCLUDE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_INT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 58)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons(((builtin_tokens.offset, "INT not match"),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("INT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_IOR(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 26)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote IOR not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("IOR", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_JUMP(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 10)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote JUMP not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("JUMP", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_JUMPUNLESS(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 12)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote JUMP-UNLESS not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("JUMPUNLESS", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_JUMPWHEN(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 11)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote JUMP-WHEN not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("JUMPWHEN", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_LABEL(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 8)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote LABEL not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("LABEL", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_LBRACKET(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 63)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote [ not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("LBRACKET", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_LE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 40)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote LE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("LE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_LOAD(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 41)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote LOAD not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("LOAD", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_LPAREN(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 0)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote ( not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("LPAREN", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_LT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 39)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote LT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("LT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_MATRIX(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 50)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote MATRIX not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("MATRIX", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_MEASURE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 7)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote MEASURE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("MEASURE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_MINUS(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 3)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote - not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("MINUS", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_MOVE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 33)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote MOVE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("MOVE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_MUL(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 31)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote MUL not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("MUL", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_NEG(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 21)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote NEG not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("NEG", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_NEWLINE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 70)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "NEWLINE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("NEWLINE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_NOP(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 15)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote NOP not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("NOP", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_NOT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 22)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote NOT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("NOT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_OFFSET(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 20)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote OFFSET not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("OFFSET", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_OR(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 28)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote OR not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("OR", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_PERCENTAGE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 66)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote % not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("PERCENTAGE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_PERMUTATION(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 51)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote PERMUTATION not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("PERMUTATION", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_PI(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 43)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote pi not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("PI", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_PLUS(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 2)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote + not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("PLUS", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_POWER(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 54)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote ^ not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("POWER", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_PRAGMA(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 17)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote PRAGMA not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("PRAGMA", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_RBRACKET(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 64)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote ] not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("RBRACKET", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_RESET(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 13)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote RESET not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("RESET", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_RPAREN(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 1)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote ) not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("RPAREN", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_SHARING(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 19)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote SHARING not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("SHARING", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_SIN(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 45)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote SIN not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("SIN", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_SQRT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 47)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote SQRT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("SQRT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_START(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 27
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 14
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 23
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 30
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 42
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 13
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 17
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 28
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 22
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 15
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 21
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 31
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 33
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 7
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 39
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 41
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 40
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 8
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 11
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 12
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 10
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 26
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 16
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 9
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 37
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 38
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 24
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 34
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 36
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 32
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 5
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 6
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 18
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 56
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 35
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 55
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 25
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 29
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 70
        rbnf_named__check_0 =  rbnf_named_parse_NEWLINE(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 70
                      rbnf_named__check_2 =  rbnf_named_parse_NEWLINE(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("START",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      rbnf_tmp_1_ =  ((),)
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_),))
                      ((True, rbnf_tmp_2_),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "START got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 57
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_0(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 70
                  rbnf_named__check_1 =  rbnf_named_parse_NEWLINE(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_2_ =  builtin_mk_ast("START",
                                     ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_2_),)
                  end
                @default
                  rbnf_tmp_2_ =  ((),)
                  rbnf_tmp_3_ =  builtin_mk_ast("START",
                                 ((rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_2_),))
                  ((True, rbnf_tmp_3_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "START got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "START lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "START got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_STORE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 42)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote STORE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("STORE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_STRING(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 60)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "STRING not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("STRING", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_SUB(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 30)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote SUB not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("SUB", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_TIMES(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 52)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote * not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("TIMES", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_TRUE(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 23)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote TRUE not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("TRUE", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_WAIT(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 14)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote WAIT not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("WAIT", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_XOR(builtin_state, builtin_tokens)
  rbnf_tmp_0 =  builtin_match_tk(builtin_tokens, 27)
  if builtin_is_null(rbnf_tmp_0)
      (( False
      , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                    , "quote XOR not match" ),),
                       builtin_nil)) ),)
  else
      rbnf_tmp_1_ =  builtin_mk_ast("XOR", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_addr(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 63
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_LBRACKET(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_INT(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_RBRACKET(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_2_ =  builtin_mk_ast("addr",
                                   (( rbnf_tmp_1_
                                   , rbnf_tmp_0
                                   , rbnf_tmp_1
                                   , rbnf_tmp_2 ),))
                    ((True, rbnf_tmp_2_),)
                end
            end
        end
      @case 57
        rbnf_named__check_0 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 63
                  rbnf_named__check_1 =  rbnf_named_parse_LBRACKET(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_named__check_2 =  rbnf_named_parse_INT(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_named__check_3 =  rbnf_named_parse_RBRACKET(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("addr",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                      end
                  end
                @default
                  rbnf_tmp_1_ =  builtin_mk_ast("addr", ((rbnf_tmp_0),))
                  ((True, rbnf_tmp_1_),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "addr got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "addr lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "addr got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_allInstr(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 27
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 14
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 23
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 30
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 42
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 13
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 17
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 28
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 22
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 15
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 21
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 31
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 33
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 7
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 39
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 41
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 40
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 8
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 11
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 12
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 10
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 26
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 16
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 9
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 37
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 38
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 24
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 34
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 36
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 32
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 5
        rbnf_named__check_0 =  rbnf_named_parse_defGate(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 6
        rbnf_named__check_0 =  rbnf_named_parse_defCircuit(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 18
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 56
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 35
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 55
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 25
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 29
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 57
        rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("allInstr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "allInstr lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "allInstr got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_arithmeticBinaryOp(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 30
        rbnf_named__check_0 =  rbnf_named_parse_SUB(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 43
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 44
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 3
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 59
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "arithmeticBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "arithmeticBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 31
        rbnf_named__check_0 =  rbnf_named_parse_MUL(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 43
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 44
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 3
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 59
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "arithmeticBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "arithmeticBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 32
        rbnf_named__check_0 =  rbnf_named_parse_DIV(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 43
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 44
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 3
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 59
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "arithmeticBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "arithmeticBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 29
        rbnf_named__check_0 =  rbnf_named_parse_ADD(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 43
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 44
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 3
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 59
                      rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("arithmeticBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "arithmeticBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "arithmeticBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "arithmeticBinaryOp lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "arithmeticBinaryOp got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_circuit(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_8(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("circuit", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_classicalBinary(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 27
        rbnf_named__check_0 =  rbnf_named_parse_logicalBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 30
        rbnf_named__check_0 =  rbnf_named_parse_arithmeticBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 28
        rbnf_named__check_0 =  rbnf_named_parse_logicalBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 31
        rbnf_named__check_0 =  rbnf_named_parse_arithmeticBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 33
        rbnf_named__check_0 =  rbnf_named_parse_move(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 26
        rbnf_named__check_0 =  rbnf_named_parse_logicalBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 34
        rbnf_named__check_0 =  rbnf_named_parse_exchange(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 32
        rbnf_named__check_0 =  rbnf_named_parse_arithmeticBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 35
        rbnf_named__check_0 =  rbnf_named_parse_convert(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 25
        rbnf_named__check_0 =  rbnf_named_parse_logicalBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 29
        rbnf_named__check_0 =  rbnf_named_parse_arithmeticBinaryOp(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("classicalBinary", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "classicalBinary lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "classicalBinary got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_classicalComparison(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 39
        rbnf_named__check_0 =  rbnf_named_parse_LT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 43
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 44
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 63
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 3
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 58
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 57
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 59
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          (( False
                          , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                        , "classicalComparison lookahead failed" ),),
                                           builtin_nil)) ),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "classicalComparison got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
            end
        end
      @case 40
        rbnf_named__check_0 =  rbnf_named_parse_LE(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 43
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 44
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 63
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 3
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 58
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 57
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 59
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          (( False
                          , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                        , "classicalComparison lookahead failed" ),),
                                           builtin_nil)) ),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "classicalComparison got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
            end
        end
      @case 37
        rbnf_named__check_0 =  rbnf_named_parse_GT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 43
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 44
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 63
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 3
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 58
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 57
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 59
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          (( False
                          , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                        , "classicalComparison lookahead failed" ),),
                                           builtin_nil)) ),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "classicalComparison got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
            end
        end
      @case 38
        rbnf_named__check_0 =  rbnf_named_parse_GE(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 43
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 44
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 63
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 3
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 58
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 57
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 59
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          (( False
                          , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                        , "classicalComparison lookahead failed" ),),
                                           builtin_nil)) ),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "classicalComparison got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
            end
        end
      @case 36
        rbnf_named__check_0 =  rbnf_named_parse_EQ(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 43
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 44
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 63
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 3
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 58
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 57
                          rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @case 59
                          rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("classicalComparison",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          (( False
                          , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                        , "classicalComparison lookahead failed" ),),
                                           builtin_nil)) ),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "classicalComparison got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "classicalComparison lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "classicalComparison got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_classicalUnary(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 23
        rbnf_named__check_0 =  rbnf_named_parse_TRUE(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("classicalUnary",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 22
        rbnf_named__check_0 =  rbnf_named_parse_NOT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("classicalUnary",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 21
        rbnf_named__check_0 =  rbnf_named_parse_NEG(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("classicalUnary",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 24
        rbnf_named__check_0 =  rbnf_named_parse_FALSE(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("classicalUnary",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "classicalUnary lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "classicalUnary got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_convert(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_CONVERT(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_tmp_1_ =  builtin_mk_ast("convert",
                             ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
              ((True, rbnf_tmp_1_),)
          end
      end
  end
end
function rbnf_named_parse_defCircuit(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_DEFCIRCUIT(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_name(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__off_0 =  builtin_tokens.offset
          if builtin_peekable(builtin_tokens, 0)
              @switch  builtin_peek(builtin_tokens, 0).idint begin
              @case 65
                rbnf_tmp_1_ =  ((),)
                rbnf_named__off_1 =  builtin_tokens.offset
                rbnf_tmp_2_ =  ((),)
                rbnf_named__check_2 =  rbnf_named_parse_COLON(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_NEWLINE(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__check_4 =  rbnf_named_parse_circuit(builtin_state,
                                               builtin_tokens)
                        if builtin_eq(rbnf_named__check_4[0], False)
                            rbnf_named__check_4
                        else
                            rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                            rbnf_tmp_3_ =  builtin_mk_ast("defCircuit",
                                           (( rbnf_tmp_0
                                           , rbnf_tmp_1
                                           , rbnf_tmp_1_
                                           , rbnf_tmp_2_
                                           , rbnf_tmp_2
                                           , rbnf_tmp_3
                                           , rbnf_tmp_4 ),))
                            ((True, rbnf_tmp_3_),)
                        end
                    end
                end
              @case 0
                rbnf_named__check_2 =  rbnf_named_parse_LPAREN(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_variable(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__off_1 =  builtin_tokens.offset
                        if builtin_peekable(builtin_tokens, 0)
                            @switch  builtin_peek(builtin_tokens, 0).idint begin
                            @case 62
                              rbnf_named__check_4 =  rbnf_named_parse_rbnfmacro_4(builtin_state,
                                                     builtin_tokens)
                              if builtin_eq(rbnf_named__check_4[0], False)
                                  rbnf_named__check_4
                              else
                                  rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                  rbnf_named__check_5 =  rbnf_named_parse_RPAREN(builtin_state,
                                                         builtin_tokens)
                                  if builtin_eq(rbnf_named__check_5[0], False)
                                      rbnf_named__check_5
                                  else
                                      rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                      rbnf_tmp_1_ =  (( rbnf_tmp_2
                                                     , rbnf_tmp_3
                                                     , rbnf_tmp_4
                                                     , rbnf_tmp_5 ),)
                                      rbnf_named__off_2 =  builtin_tokens.offset
                                      if builtin_peekable(builtin_tokens, 0)
                                          @switch  builtin_peek(builtin_tokens,
                                                   0).idint begin
                                          @case 65
                                            rbnf_tmp_2_ =  ((),)
                                            rbnf_named__check_6 =  rbnf_named_parse_COLON(builtin_state,
                                                                   builtin_tokens)
                                            if builtin_eq(rbnf_named__check_6[0],
                                               False)
                                                rbnf_named__check_6
                                            else
                                                rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                                rbnf_named__check_7 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                                       builtin_tokens)
                                                if builtin_eq(rbnf_named__check_7[0],
                                                   False)
                                                    rbnf_named__check_7
                                                else
                                                    rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                                    rbnf_named__check_8 =  rbnf_named_parse_circuit(builtin_state,
                                                                           builtin_tokens)
                                                    if builtin_eq(rbnf_named__check_8[0],
                                                       False)
                                                        rbnf_named__check_8
                                                    else
                                                        rbnf_tmp_8 =  builtin_to_result(rbnf_named__check_8[1])
                                                        rbnf_tmp_3_ =  builtin_mk_ast("defCircuit",
                                                                       (( rbnf_tmp_0
                                                                       , rbnf_tmp_1
                                                                       , rbnf_tmp_1_
                                                                       , rbnf_tmp_2_
                                                                       , rbnf_tmp_6
                                                                       , rbnf_tmp_7
                                                                       , rbnf_tmp_8 ),))
                                                        ((True, rbnf_tmp_3_),)
                                                    end
                                                end
                                            end
                                          @case 57
                                            rbnf_named__check_6 =  rbnf_named_parse_rbnfmacro_7(builtin_state,
                                                                   builtin_tokens)
                                            if builtin_eq(rbnf_named__check_6[0],
                                               False)
                                                rbnf_named__check_6
                                            else
                                                rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                                rbnf_named__check_7 =  rbnf_named_parse_COLON(builtin_state,
                                                                       builtin_tokens)
                                                if builtin_eq(rbnf_named__check_7[0],
                                                   False)
                                                    rbnf_named__check_7
                                                else
                                                    rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                                    rbnf_named__check_8 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                                           builtin_tokens)
                                                    if builtin_eq(rbnf_named__check_8[0],
                                                       False)
                                                        rbnf_named__check_8
                                                    else
                                                        rbnf_tmp_8 =  builtin_to_result(rbnf_named__check_8[1])
                                                        rbnf_named__check_9 =  rbnf_named_parse_circuit(builtin_state,
                                                                               builtin_tokens)
                                                        if builtin_eq(rbnf_named__check_9[0],
                                                           False)
                                                            rbnf_named__check_9
                                                        else
                                                            rbnf_tmp_9 =  builtin_to_result(rbnf_named__check_9[1])
                                                            rbnf_tmp_2_ =  builtin_mk_ast("defCircuit",
                                                                           (( rbnf_tmp_0
                                                                           , rbnf_tmp_1
                                                                           , rbnf_tmp_1_
                                                                           , rbnf_tmp_6
                                                                           , rbnf_tmp_7
                                                                           , rbnf_tmp_8
                                                                           , rbnf_tmp_9 ),))
                                                            (( True
                                                            , rbnf_tmp_2_ ),)
                                                        end
                                                    end
                                                end
                                            end
                                          @default
                                            (( False
                                            , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                          , "defCircuit lookahead failed" ),),
                                                             builtin_nil)) ),)
                                          end
                                      else
                                          (( False
                                          , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                        , "defCircuit got EOF" ),),
                                                           builtin_nil)) ),)
                                      end
                                  end
                              end
                            @case 1
                              rbnf_tmp_1_ =  ((),)
                              rbnf_named__check_4 =  rbnf_named_parse_RPAREN(builtin_state,
                                                     builtin_tokens)
                              if builtin_eq(rbnf_named__check_4[0], False)
                                  rbnf_named__check_4
                              else
                                  rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                  rbnf_tmp_2_ =  (( rbnf_tmp_2
                                                 , rbnf_tmp_3
                                                 , rbnf_tmp_1_
                                                 , rbnf_tmp_4 ),)
                                  rbnf_named__off_2 =  builtin_tokens.offset
                                  if builtin_peekable(builtin_tokens, 0)
                                      @switch  builtin_peek(builtin_tokens,
                                               0).idint begin
                                      @case 65
                                        rbnf_tmp_3_ =  ((),)
                                        rbnf_named__check_5 =  rbnf_named_parse_COLON(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_named__check_6 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                                   builtin_tokens)
                                            if builtin_eq(rbnf_named__check_6[0],
                                               False)
                                                rbnf_named__check_6
                                            else
                                                rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                                rbnf_named__check_7 =  rbnf_named_parse_circuit(builtin_state,
                                                                       builtin_tokens)
                                                if builtin_eq(rbnf_named__check_7[0],
                                                   False)
                                                    rbnf_named__check_7
                                                else
                                                    rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                                    rbnf_tmp_4_ =  builtin_mk_ast("defCircuit",
                                                                   (( rbnf_tmp_0
                                                                   , rbnf_tmp_1
                                                                   , rbnf_tmp_2_
                                                                   , rbnf_tmp_3_
                                                                   , rbnf_tmp_5
                                                                   , rbnf_tmp_6
                                                                   , rbnf_tmp_7 ),))
                                                    ((True, rbnf_tmp_4_),)
                                                end
                                            end
                                        end
                                      @case 57
                                        rbnf_named__check_5 =  rbnf_named_parse_rbnfmacro_7(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_named__check_6 =  rbnf_named_parse_COLON(builtin_state,
                                                                   builtin_tokens)
                                            if builtin_eq(rbnf_named__check_6[0],
                                               False)
                                                rbnf_named__check_6
                                            else
                                                rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                                rbnf_named__check_7 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                                       builtin_tokens)
                                                if builtin_eq(rbnf_named__check_7[0],
                                                   False)
                                                    rbnf_named__check_7
                                                else
                                                    rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                                    rbnf_named__check_8 =  rbnf_named_parse_circuit(builtin_state,
                                                                           builtin_tokens)
                                                    if builtin_eq(rbnf_named__check_8[0],
                                                       False)
                                                        rbnf_named__check_8
                                                    else
                                                        rbnf_tmp_8 =  builtin_to_result(rbnf_named__check_8[1])
                                                        rbnf_tmp_3_ =  builtin_mk_ast("defCircuit",
                                                                       (( rbnf_tmp_0
                                                                       , rbnf_tmp_1
                                                                       , rbnf_tmp_2_
                                                                       , rbnf_tmp_5
                                                                       , rbnf_tmp_6
                                                                       , rbnf_tmp_7
                                                                       , rbnf_tmp_8 ),))
                                                        ((True, rbnf_tmp_3_),)
                                                    end
                                                end
                                            end
                                        end
                                      @default
                                        (( False
                                        , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                      , "defCircuit lookahead failed" ),),
                                                         builtin_nil)) ),)
                                      end
                                  else
                                      (( False
                                      , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                    , "defCircuit got EOF" ),),
                                                       builtin_nil)) ),)
                                  end
                              end
                            @default
                              (( False
                              , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                            , "defCircuit lookahead failed" ),),
                                               builtin_nil)) ),)
                            end
                        else
                            (( False
                            , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                          , "defCircuit got EOF" ),),
                                             builtin_nil)) ),)
                        end
                    end
                end
              @case 57
                rbnf_tmp_1_ =  ((),)
                rbnf_named__off_1 =  builtin_tokens.offset
                rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_7(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_COLON(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__check_4 =  rbnf_named_parse_NEWLINE(builtin_state,
                                               builtin_tokens)
                        if builtin_eq(rbnf_named__check_4[0], False)
                            rbnf_named__check_4
                        else
                            rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                            rbnf_named__check_5 =  rbnf_named_parse_circuit(builtin_state,
                                                   builtin_tokens)
                            if builtin_eq(rbnf_named__check_5[0], False)
                                rbnf_named__check_5
                            else
                                rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                rbnf_tmp_2_ =  builtin_mk_ast("defCircuit",
                                               (( rbnf_tmp_0
                                               , rbnf_tmp_1
                                               , rbnf_tmp_1_
                                               , rbnf_tmp_2
                                               , rbnf_tmp_3
                                               , rbnf_tmp_4
                                               , rbnf_tmp_5 ),))
                                ((True, rbnf_tmp_2_),)
                            end
                        end
                    end
                end
              @default
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                              , "defCircuit lookahead failed" ),),
                                 builtin_nil)) ),)
              end
          else
              (( False
              , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                            , "defCircuit got EOF" ),),
                               builtin_nil)) ),)
          end
      end
  end
end
function rbnf_named_parse_defGate(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_DEFGATE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_name(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__off_0 =  builtin_tokens.offset
          if builtin_peekable(builtin_tokens, 0)
              @switch  builtin_peek(builtin_tokens, 0).idint begin
              @case 4
                rbnf_named__check_2 =  rbnf_named_parse_AS(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_gateType(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  ((rbnf_tmp_2, rbnf_tmp_3),)
                        rbnf_named__check_4 =  rbnf_named_parse_COLON(builtin_state,
                                               builtin_tokens)
                        if builtin_eq(rbnf_named__check_4[0], False)
                            rbnf_named__check_4
                        else
                            rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                            rbnf_named__check_5 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                   builtin_tokens)
                            if builtin_eq(rbnf_named__check_5[0], False)
                                rbnf_named__check_5
                            else
                                rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                rbnf_named__check_6 =  rbnf_named_parse_matrix(builtin_state,
                                                       builtin_tokens)
                                if builtin_eq(rbnf_named__check_6[0], False)
                                    rbnf_named__check_6
                                else
                                    rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                    rbnf_tmp_2_ =  builtin_mk_ast("defGate",
                                                   (( rbnf_tmp_0
                                                   , rbnf_tmp_1
                                                   , rbnf_tmp_1_
                                                   , rbnf_tmp_4
                                                   , rbnf_tmp_5
                                                   , rbnf_tmp_6 ),))
                                    ((True, rbnf_tmp_2_),)
                                end
                            end
                        end
                    end
                end
              @case 65
                rbnf_tmp_1_ =  ((),)
                rbnf_named__check_2 =  rbnf_named_parse_COLON(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_NEWLINE(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__check_4 =  rbnf_named_parse_matrix(builtin_state,
                                               builtin_tokens)
                        if builtin_eq(rbnf_named__check_4[0], False)
                            rbnf_named__check_4
                        else
                            rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                            rbnf_tmp_2_ =  builtin_mk_ast("defGate",
                                           (( rbnf_tmp_0
                                           , rbnf_tmp_1
                                           , rbnf_tmp_1_
                                           , rbnf_tmp_2
                                           , rbnf_tmp_3
                                           , rbnf_tmp_4 ),))
                            ((True, rbnf_tmp_2_),)
                        end
                    end
                end
              @case 0
                rbnf_named__check_2 =  rbnf_named_parse_LPAREN(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_variable(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__off_1 =  builtin_tokens.offset
                        if builtin_peekable(builtin_tokens, 0)
                            @switch  builtin_peek(builtin_tokens, 0).idint begin
                            @case 62
                              rbnf_named__check_4 =  rbnf_named_parse_rbnfmacro_4(builtin_state,
                                                     builtin_tokens)
                              if builtin_eq(rbnf_named__check_4[0], False)
                                  rbnf_named__check_4
                              else
                                  rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                  rbnf_named__check_5 =  rbnf_named_parse_RPAREN(builtin_state,
                                                         builtin_tokens)
                                  if builtin_eq(rbnf_named__check_5[0], False)
                                      rbnf_named__check_5
                                  else
                                      rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                      rbnf_tmp_1_ =  (( rbnf_tmp_2
                                                     , rbnf_tmp_3
                                                     , rbnf_tmp_4
                                                     , rbnf_tmp_5 ),)
                                      rbnf_named__check_6 =  rbnf_named_parse_COLON(builtin_state,
                                                             builtin_tokens)
                                      if builtin_eq(rbnf_named__check_6[0],
                                         False)
                                          rbnf_named__check_6
                                      else
                                          rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                          rbnf_named__check_7 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                                 builtin_tokens)
                                          if builtin_eq(rbnf_named__check_7[0],
                                             False)
                                              rbnf_named__check_7
                                          else
                                              rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                              rbnf_named__check_8 =  rbnf_named_parse_matrix(builtin_state,
                                                                     builtin_tokens)
                                              if builtin_eq(rbnf_named__check_8[0],
                                                 False)
                                                  rbnf_named__check_8
                                              else
                                                  rbnf_tmp_8 =  builtin_to_result(rbnf_named__check_8[1])
                                                  rbnf_tmp_2_ =  builtin_mk_ast("defGate",
                                                                 (( rbnf_tmp_0
                                                                 , rbnf_tmp_1
                                                                 , rbnf_tmp_1_
                                                                 , rbnf_tmp_6
                                                                 , rbnf_tmp_7
                                                                 , rbnf_tmp_8 ),))
                                                  ((True, rbnf_tmp_2_),)
                                              end
                                          end
                                      end
                                  end
                              end
                            @case 1
                              rbnf_tmp_1_ =  ((),)
                              rbnf_named__check_4 =  rbnf_named_parse_RPAREN(builtin_state,
                                                     builtin_tokens)
                              if builtin_eq(rbnf_named__check_4[0], False)
                                  rbnf_named__check_4
                              else
                                  rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                  rbnf_tmp_2_ =  (( rbnf_tmp_2
                                                 , rbnf_tmp_3
                                                 , rbnf_tmp_1_
                                                 , rbnf_tmp_4 ),)
                                  rbnf_named__check_5 =  rbnf_named_parse_COLON(builtin_state,
                                                         builtin_tokens)
                                  if builtin_eq(rbnf_named__check_5[0], False)
                                      rbnf_named__check_5
                                  else
                                      rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                      rbnf_named__check_6 =  rbnf_named_parse_NEWLINE(builtin_state,
                                                             builtin_tokens)
                                      if builtin_eq(rbnf_named__check_6[0],
                                         False)
                                          rbnf_named__check_6
                                      else
                                          rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                          rbnf_named__check_7 =  rbnf_named_parse_matrix(builtin_state,
                                                                 builtin_tokens)
                                          if builtin_eq(rbnf_named__check_7[0],
                                             False)
                                              rbnf_named__check_7
                                          else
                                              rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                              rbnf_tmp_3_ =  builtin_mk_ast("defGate",
                                                             (( rbnf_tmp_0
                                                             , rbnf_tmp_1
                                                             , rbnf_tmp_2_
                                                             , rbnf_tmp_5
                                                             , rbnf_tmp_6
                                                             , rbnf_tmp_7 ),))
                                              ((True, rbnf_tmp_3_),)
                                          end
                                      end
                                  end
                              end
                            @default
                              (( False
                              , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                            , "defGate lookahead failed" ),),
                                               builtin_nil)) ),)
                            end
                        else
                            (( False
                            , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                          , "defGate got EOF" ),),
                                             builtin_nil)) ),)
                        end
                    end
                end
              @default
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                              , "defGate lookahead failed" ),),
                                 builtin_nil)) ),)
              end
          else
              (( False
              , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                            , "defGate got EOF" ),),
                               builtin_nil)) ),)
          end
      end
  end
end
function rbnf_named_parse_defLabel(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_LABEL(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_label(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_mk_ast("defLabel", ((rbnf_tmp_0, rbnf_tmp_1),))
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_parse_exchange(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_EXCHANGE(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_tmp_1_ =  builtin_mk_ast("exchange",
                             ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
              ((True, rbnf_tmp_1_),)
          end
      end
  end
end
function rbnf_named_parse_expression(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_nonPowerExp(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__off_0 =  builtin_tokens.offset
      if builtin_peekable(builtin_tokens, 0)
          @switch  builtin_peek(builtin_tokens, 0).idint begin
          @case 54
            rbnf_named__check_1 =  rbnf_named_parse_POWER(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("expression",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    rbnf_named_lr_loop_expression(rbnf_tmp_1_,
                    builtin_state,
                    builtin_tokens)
                end
            end
          @case 53
            rbnf_tmp_1_ =  rbnf_tmp_0
            rbnf_named_lr_loop_expression(rbnf_tmp_1_,
            builtin_state,
            builtin_tokens)
          @case 3
            rbnf_tmp_1_ =  rbnf_tmp_0
            rbnf_named_lr_loop_expression(rbnf_tmp_1_,
            builtin_state,
            builtin_tokens)
          @case 2
            rbnf_tmp_1_ =  rbnf_tmp_0
            rbnf_named_lr_loop_expression(rbnf_tmp_1_,
            builtin_state,
            builtin_tokens)
          @case 52
            rbnf_tmp_1_ =  rbnf_tmp_0
            rbnf_named_lr_loop_expression(rbnf_tmp_1_,
            builtin_state,
            builtin_tokens)
          @default
            rbnf_tmp_1_ =  rbnf_tmp_0
            rbnf_named_lr_loop_expression(rbnf_tmp_1_,
            builtin_state,
            builtin_tokens)
          end
      else
          (( False
          , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                        , "expression got EOF" ),),
                           builtin_nil)) ),)
      end
  end
end
function rbnf_named_parse_function(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 47
        rbnf_named__check_0 =  rbnf_named_parse_SQRT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("function", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 45
        rbnf_named__check_0 =  rbnf_named_parse_SIN(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("function", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 48
        rbnf_named__check_0 =  rbnf_named_parse_EXP(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("function", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 46
        rbnf_named__check_0 =  rbnf_named_parse_COS(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("function", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 49
        rbnf_named__check_0 =  rbnf_named_parse_CIS(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("function", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "function lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "function got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_gate(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 56
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_1(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_name(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 0
                      rbnf_named__check_2 =  rbnf_named_parse_LPAREN(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_named__check_3 =  rbnf_named_parse_param(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_named__off_2 =  builtin_tokens.offset
                              if builtin_peekable(builtin_tokens, 0)
                                  @switch  builtin_peek(builtin_tokens,
                                           0).idint begin
                                  @case 62
                                    rbnf_named__check_4 =  rbnf_named_parse_rbnfmacro_2(builtin_state,
                                                           builtin_tokens)
                                    if builtin_eq(rbnf_named__check_4[0], False)
                                        rbnf_named__check_4
                                    else
                                        rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                        rbnf_named__check_5 =  rbnf_named_parse_RPAREN(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_tmp_1_ =  (( rbnf_tmp_2
                                                           , rbnf_tmp_3
                                                           , rbnf_tmp_4
                                                           , rbnf_tmp_5 ),)
                                            rbnf_named__check_6 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                                                   builtin_tokens)
                                            if builtin_eq(rbnf_named__check_6[0],
                                               False)
                                                rbnf_named__check_6
                                            else
                                                rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                                rbnf_tmp_2_ =  builtin_mk_ast("gate",
                                                               (( rbnf_tmp_0
                                                               , rbnf_tmp_1
                                                               , rbnf_tmp_1_
                                                               , rbnf_tmp_6 ),))
                                                ((True, rbnf_tmp_2_),)
                                            end
                                        end
                                    end
                                  @case 1
                                    rbnf_tmp_1_ =  ((),)
                                    rbnf_named__check_4 =  rbnf_named_parse_RPAREN(builtin_state,
                                                           builtin_tokens)
                                    if builtin_eq(rbnf_named__check_4[0], False)
                                        rbnf_named__check_4
                                    else
                                        rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                        rbnf_tmp_2_ =  (( rbnf_tmp_2
                                                       , rbnf_tmp_3
                                                       , rbnf_tmp_1_
                                                       , rbnf_tmp_4 ),)
                                        rbnf_named__check_5 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_tmp_3_ =  builtin_mk_ast("gate",
                                                           (( rbnf_tmp_0
                                                           , rbnf_tmp_1
                                                           , rbnf_tmp_2_
                                                           , rbnf_tmp_5 ),))
                                            ((True, rbnf_tmp_3_),)
                                        end
                                    end
                                  @default
                                    (( False
                                    , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                  , "gate lookahead failed" ),),
                                                     builtin_nil)) ),)
                                  end
                              else
                                  (( False
                                  , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                , "gate got EOF" ),),
                                                   builtin_nil)) ),)
                              end
                          end
                      end
                    @case 58
                      rbnf_tmp_1_ =  ((),)
                      rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_2_ =  builtin_mk_ast("gate",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_1_
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_2_),)
                      end
                    @case 57
                      rbnf_tmp_1_ =  ((),)
                      rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_2_ =  builtin_mk_ast("gate",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_1_
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_2_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "gate lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "gate got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 55
        rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_1(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_name(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 0
                      rbnf_named__check_2 =  rbnf_named_parse_LPAREN(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_named__check_3 =  rbnf_named_parse_param(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_named__off_2 =  builtin_tokens.offset
                              if builtin_peekable(builtin_tokens, 0)
                                  @switch  builtin_peek(builtin_tokens,
                                           0).idint begin
                                  @case 62
                                    rbnf_named__check_4 =  rbnf_named_parse_rbnfmacro_2(builtin_state,
                                                           builtin_tokens)
                                    if builtin_eq(rbnf_named__check_4[0], False)
                                        rbnf_named__check_4
                                    else
                                        rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                        rbnf_named__check_5 =  rbnf_named_parse_RPAREN(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_tmp_1_ =  (( rbnf_tmp_2
                                                           , rbnf_tmp_3
                                                           , rbnf_tmp_4
                                                           , rbnf_tmp_5 ),)
                                            rbnf_named__check_6 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                                                   builtin_tokens)
                                            if builtin_eq(rbnf_named__check_6[0],
                                               False)
                                                rbnf_named__check_6
                                            else
                                                rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                                rbnf_tmp_2_ =  builtin_mk_ast("gate",
                                                               (( rbnf_tmp_0
                                                               , rbnf_tmp_1
                                                               , rbnf_tmp_1_
                                                               , rbnf_tmp_6 ),))
                                                ((True, rbnf_tmp_2_),)
                                            end
                                        end
                                    end
                                  @case 1
                                    rbnf_tmp_1_ =  ((),)
                                    rbnf_named__check_4 =  rbnf_named_parse_RPAREN(builtin_state,
                                                           builtin_tokens)
                                    if builtin_eq(rbnf_named__check_4[0], False)
                                        rbnf_named__check_4
                                    else
                                        rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                        rbnf_tmp_2_ =  (( rbnf_tmp_2
                                                       , rbnf_tmp_3
                                                       , rbnf_tmp_1_
                                                       , rbnf_tmp_4 ),)
                                        rbnf_named__check_5 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_tmp_3_ =  builtin_mk_ast("gate",
                                                           (( rbnf_tmp_0
                                                           , rbnf_tmp_1
                                                           , rbnf_tmp_2_
                                                           , rbnf_tmp_5 ),))
                                            ((True, rbnf_tmp_3_),)
                                        end
                                    end
                                  @default
                                    (( False
                                    , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                  , "gate lookahead failed" ),),
                                                     builtin_nil)) ),)
                                  end
                              else
                                  (( False
                                  , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                , "gate got EOF" ),),
                                                   builtin_nil)) ),)
                              end
                          end
                      end
                    @case 58
                      rbnf_tmp_1_ =  ((),)
                      rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_2_ =  builtin_mk_ast("gate",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_1_
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_2_),)
                      end
                    @case 57
                      rbnf_tmp_1_ =  ((),)
                      rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_2_ =  builtin_mk_ast("gate",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_1_
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_2_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "gate lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "gate got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 57
        rbnf_tmp_1_ =  ((),)
        rbnf_named__check_0 =  rbnf_named_parse_name(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 0
                  rbnf_named__check_1 =  rbnf_named_parse_LPAREN(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_named__check_2 =  rbnf_named_parse_param(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_named__off_2 =  builtin_tokens.offset
                          if builtin_peekable(builtin_tokens, 0)
                              @switch  builtin_peek(builtin_tokens,
                                       0).idint begin
                              @case 62
                                rbnf_named__check_3 =  rbnf_named_parse_rbnfmacro_2(builtin_state,
                                                       builtin_tokens)
                                if builtin_eq(rbnf_named__check_3[0], False)
                                    rbnf_named__check_3
                                else
                                    rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                                    rbnf_named__check_4 =  rbnf_named_parse_RPAREN(builtin_state,
                                                           builtin_tokens)
                                    if builtin_eq(rbnf_named__check_4[0], False)
                                        rbnf_named__check_4
                                    else
                                        rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                        rbnf_tmp_2_ =  (( rbnf_tmp_1
                                                       , rbnf_tmp_2
                                                       , rbnf_tmp_3
                                                       , rbnf_tmp_4 ),)
                                        rbnf_named__check_5 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_tmp_3_ =  builtin_mk_ast("gate",
                                                           (( rbnf_tmp_1_
                                                           , rbnf_tmp_0
                                                           , rbnf_tmp_2_
                                                           , rbnf_tmp_5 ),))
                                            ((True, rbnf_tmp_3_),)
                                        end
                                    end
                                end
                              @case 1
                                rbnf_tmp_2_ =  ((),)
                                rbnf_named__check_3 =  rbnf_named_parse_RPAREN(builtin_state,
                                                       builtin_tokens)
                                if builtin_eq(rbnf_named__check_3[0], False)
                                    rbnf_named__check_3
                                else
                                    rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                                    rbnf_tmp_3_ =  (( rbnf_tmp_1
                                                   , rbnf_tmp_2
                                                   , rbnf_tmp_2_
                                                   , rbnf_tmp_3 ),)
                                    rbnf_named__check_4 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                                           builtin_tokens)
                                    if builtin_eq(rbnf_named__check_4[0], False)
                                        rbnf_named__check_4
                                    else
                                        rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                        rbnf_tmp_4_ =  builtin_mk_ast("gate",
                                                       (( rbnf_tmp_1_
                                                       , rbnf_tmp_0
                                                       , rbnf_tmp_3_
                                                       , rbnf_tmp_4 ),))
                                        ((True, rbnf_tmp_4_),)
                                    end
                                end
                              @default
                                (( False
                                , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                              , "gate lookahead failed" ),),
                                                 builtin_nil)) ),)
                              end
                          else
                              (( False
                              , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                            , "gate got EOF" ),),
                                               builtin_nil)) ),)
                          end
                      end
                  end
                @case 58
                  rbnf_tmp_2_ =  ((),)
                  rbnf_named__check_1 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_3_ =  builtin_mk_ast("gate",
                                     (( rbnf_tmp_1_
                                     , rbnf_tmp_0
                                     , rbnf_tmp_2_
                                     , rbnf_tmp_1 ),))
                      ((True, rbnf_tmp_3_),)
                  end
                @case 57
                  rbnf_tmp_2_ =  ((),)
                  rbnf_named__check_1 =  rbnf_named_parse_rbnfmacro_3(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_3_ =  builtin_mk_ast("gate",
                                     (( rbnf_tmp_1_
                                     , rbnf_tmp_0
                                     , rbnf_tmp_2_
                                     , rbnf_tmp_1 ),))
                      ((True, rbnf_tmp_3_),)
                  end
                @default
                  (( False
                  , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                , "gate lookahead failed" ),),
                                   builtin_nil)) ),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "gate got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "gate lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "gate got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_gateType(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 51
        rbnf_named__check_0 =  rbnf_named_parse_PERMUTATION(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("gateType", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 50
        rbnf_named__check_0 =  rbnf_named_parse_MATRIX(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("gateType", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "gateType lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "gateType got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_halt(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_HALT(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("halt", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_imaginaryN(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_realN(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__off_0 =  builtin_tokens.offset
      if builtin_peekable(builtin_tokens, 0)
          @switch  builtin_peek(builtin_tokens, 0).idint begin
          @case 44
            rbnf_named__check_1 =  rbnf_named_parse_I(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("imaginaryN",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
          @default
            rbnf_tmp_1_ =  builtin_mk_ast("imaginaryN", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
          end
      else
          (( False
          , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                        , "imaginaryN got EOF" ),),
                           builtin_nil)) ),)
      end
  end
end
function rbnf_named_parse_include(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_INCLUDE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_STRING(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_mk_ast("include", ((rbnf_tmp_0, rbnf_tmp_1),))
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_parse_instr(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 27
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 14
        rbnf_named__check_0 =  rbnf_named_parse_wait(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 23
        rbnf_named__check_0 =  rbnf_named_parse_classicalUnary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 30
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 42
        rbnf_named__check_0 =  rbnf_named_parse_store(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 13
        rbnf_named__check_0 =  rbnf_named_parse_resetState(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 17
        rbnf_named__check_0 =  rbnf_named_parse_pragma(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 28
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 22
        rbnf_named__check_0 =  rbnf_named_parse_classicalUnary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 15
        rbnf_named__check_0 =  rbnf_named_parse_nop(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 21
        rbnf_named__check_0 =  rbnf_named_parse_classicalUnary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 31
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 33
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 7
        rbnf_named__check_0 =  rbnf_named_parse_measure(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 39
        rbnf_named__check_0 =  rbnf_named_parse_classicalComparison(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 41
        rbnf_named__check_0 =  rbnf_named_parse_load(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 40
        rbnf_named__check_0 =  rbnf_named_parse_classicalComparison(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 8
        rbnf_named__check_0 =  rbnf_named_parse_defLabel(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 11
        rbnf_named__check_0 =  rbnf_named_parse_jumpWhen(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 12
        rbnf_named__check_0 =  rbnf_named_parse_jumpUnless(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 10
        rbnf_named__check_0 =  rbnf_named_parse_jump(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 26
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 16
        rbnf_named__check_0 =  rbnf_named_parse_include(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 9
        rbnf_named__check_0 =  rbnf_named_parse_halt(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 37
        rbnf_named__check_0 =  rbnf_named_parse_classicalComparison(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 38
        rbnf_named__check_0 =  rbnf_named_parse_classicalComparison(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 24
        rbnf_named__check_0 =  rbnf_named_parse_classicalUnary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 34
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 36
        rbnf_named__check_0 =  rbnf_named_parse_classicalComparison(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 32
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 18
        rbnf_named__check_0 =  rbnf_named_parse_memoryDescriptor(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 56
        rbnf_named__check_0 =  rbnf_named_parse_gate(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 35
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 55
        rbnf_named__check_0 =  rbnf_named_parse_gate(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 25
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 29
        rbnf_named__check_0 =  rbnf_named_parse_classicalBinary(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 57
        rbnf_named__check_0 =  rbnf_named_parse_gate(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("instr", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "instr lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "instr got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_jump(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_JUMP(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_label(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_mk_ast("jump", ((rbnf_tmp_0, rbnf_tmp_1),))
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_parse_jumpUnless(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_JUMPUNLESS(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_label(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_tmp_1_ =  builtin_mk_ast("jumpUnless",
                             ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
              ((True, rbnf_tmp_1_),)
          end
      end
  end
end
function rbnf_named_parse_jumpWhen(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_JUMPWHEN(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_label(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_tmp_1_ =  builtin_mk_ast("jumpWhen",
                             ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
              ((True, rbnf_tmp_1_),)
          end
      end
  end
end
function rbnf_named_parse_label(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_AT(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_mk_ast("label", ((rbnf_tmp_0, rbnf_tmp_1),))
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_parse_load(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_LOAD(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                     builtin_tokens)
              if builtin_eq(rbnf_named__check_3[0], False)
                  rbnf_named__check_3
              else
                  rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                  rbnf_tmp_1_ =  builtin_mk_ast("load",
                                 (( rbnf_tmp_0
                                 , rbnf_tmp_1
                                 , rbnf_tmp_2
                                 , rbnf_tmp_3 ),))
                  ((True, rbnf_tmp_1_),)
              end
          end
      end
  end
end
function rbnf_named_parse_logicalBinaryOp(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 27
        rbnf_named__check_0 =  rbnf_named_parse_XOR(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_INT(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "logicalBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "logicalBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 28
        rbnf_named__check_0 =  rbnf_named_parse_OR(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_INT(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "logicalBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "logicalBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 26
        rbnf_named__check_0 =  rbnf_named_parse_IOR(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_INT(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "logicalBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "logicalBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @case 25
        rbnf_named__check_0 =  rbnf_named_parse_AND(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 63
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 58
                      rbnf_named__check_2 =  rbnf_named_parse_INT(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @case 57
                      rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_1_ =  builtin_mk_ast("logicalBinaryOp",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_1_),)
                      end
                    @default
                      (( False
                      , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                    , "logicalBinaryOp lookahead failed" ),),
                                       builtin_nil)) ),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "logicalBinaryOp got EOF" ),),
                                     builtin_nil)) ),)
                end
            end
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "logicalBinaryOp lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "logicalBinaryOp got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_matrix(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_rbnfmacro_5(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("matrix", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_matrixRow(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_expression(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__off_0 =  builtin_tokens.offset
      if builtin_peekable(builtin_tokens, 0)
          @switch  builtin_peek(builtin_tokens, 0).idint begin
          @case 62
            rbnf_named__check_1 =  rbnf_named_parse_rbnfmacro_6(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("matrixRow",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
          @default
            rbnf_tmp_1_ =  ((),)
            rbnf_tmp_2_ =  builtin_mk_ast("matrixRow",
                           ((rbnf_tmp_0, rbnf_tmp_1_),))
            ((True, rbnf_tmp_2_),)
          end
      else
          (( False
          , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                        , "matrixRow got EOF" ),),
                           builtin_nil)) ),)
      end
  end
end
function rbnf_named_parse_measure(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_MEASURE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_qubit(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__off_0 =  builtin_tokens.offset
          if builtin_peekable(builtin_tokens, 0)
              @switch  builtin_peek(builtin_tokens, 0).idint begin
              @case 63
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("measure",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 57
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("measure",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @default
                rbnf_tmp_1_ =  ((),)
                rbnf_tmp_2_ =  builtin_mk_ast("measure",
                               ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_),))
                ((True, rbnf_tmp_2_),)
              end
          else
              (( False
              , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                            , "measure got EOF" ),),
                               builtin_nil)) ),)
          end
      end
  end
end
function rbnf_named_parse_memoryDescriptor(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_DECLARE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_named__off_0 =  builtin_tokens.offset
              if builtin_peekable(builtin_tokens, 0)
                  @switch  builtin_peek(builtin_tokens, 0).idint begin
                  @case 63
                    rbnf_named__check_3 =  rbnf_named_parse_LBRACKET(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__check_4 =  rbnf_named_parse_INT(builtin_state,
                                               builtin_tokens)
                        if builtin_eq(rbnf_named__check_4[0], False)
                            rbnf_named__check_4
                        else
                            rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                            rbnf_named__check_5 =  rbnf_named_parse_RBRACKET(builtin_state,
                                                   builtin_tokens)
                            if builtin_eq(rbnf_named__check_5[0], False)
                                rbnf_named__check_5
                            else
                                rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                rbnf_tmp_1_ =  (( rbnf_tmp_3
                                               , rbnf_tmp_4
                                               , rbnf_tmp_5 ),)
                                rbnf_named__off_1 =  builtin_tokens.offset
                                if builtin_peekable(builtin_tokens, 0)
                                    @switch  builtin_peek(builtin_tokens,
                                             0).idint begin
                                    @case 19
                                      rbnf_named__check_6 =  rbnf_named_parse_SHARING(builtin_state,
                                                             builtin_tokens)
                                      if builtin_eq(rbnf_named__check_6[0],
                                         False)
                                          rbnf_named__check_6
                                      else
                                          rbnf_tmp_6 =  builtin_to_result(rbnf_named__check_6[1])
                                          rbnf_named__check_7 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                                                                 builtin_tokens)
                                          if builtin_eq(rbnf_named__check_7[0],
                                             False)
                                              rbnf_named__check_7
                                          else
                                              rbnf_tmp_7 =  builtin_to_result(rbnf_named__check_7[1])
                                              rbnf_named__off_2 =  builtin_tokens.offset
                                              if builtin_peekable(builtin_tokens,
                                                 0)
                                                  @switch  builtin_peek(builtin_tokens,
                                                           0).idint begin
                                                  @case 20
                                                    rbnf_named__check_8 =  rbnf_named_parse_rbnfmacro_9(builtin_state,
                                                                           builtin_tokens)
                                                    if builtin_eq(rbnf_named__check_8[0],
                                                       False)
                                                        rbnf_named__check_8
                                                    else
                                                        rbnf_tmp_8 =  builtin_to_result(rbnf_named__check_8[1])
                                                        rbnf_tmp_2_ =  (( rbnf_tmp_6
                                                                       , rbnf_tmp_7
                                                                       , rbnf_tmp_8 ),)
                                                        rbnf_tmp_3_ =  builtin_mk_ast("memoryDescriptor",
                                                                       (( rbnf_tmp_0
                                                                       , rbnf_tmp_1
                                                                       , rbnf_tmp_2
                                                                       , rbnf_tmp_1_
                                                                       , rbnf_tmp_2_ ),))
                                                        ((True, rbnf_tmp_3_),)
                                                    end
                                                  @default
                                                    rbnf_tmp_2_ =  ((),)
                                                    rbnf_tmp_3_ =  (( rbnf_tmp_6
                                                                   , rbnf_tmp_7
                                                                   , rbnf_tmp_2_ ),)
                                                    rbnf_tmp_4_ =  builtin_mk_ast("memoryDescriptor",
                                                                   (( rbnf_tmp_0
                                                                   , rbnf_tmp_1
                                                                   , rbnf_tmp_2
                                                                   , rbnf_tmp_1_
                                                                   , rbnf_tmp_3_ ),))
                                                    ((True, rbnf_tmp_4_),)
                                                  end
                                              else
                                                  (( False
                                                  , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                                , "memoryDescriptor got EOF" ),),
                                                                   builtin_nil)) ),)
                                              end
                                          end
                                      end
                                    @default
                                      rbnf_tmp_2_ =  ((),)
                                      rbnf_tmp_3_ =  builtin_mk_ast("memoryDescriptor",
                                                     (( rbnf_tmp_0
                                                     , rbnf_tmp_1
                                                     , rbnf_tmp_2
                                                     , rbnf_tmp_1_
                                                     , rbnf_tmp_2_ ),))
                                      ((True, rbnf_tmp_3_),)
                                    end
                                else
                                    (( False
                                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                                  , "memoryDescriptor got EOF" ),),
                                                     builtin_nil)) ),)
                                end
                            end
                        end
                    end
                  @case 19
                    rbnf_tmp_1_ =  ((),)
                    rbnf_named__off_1 =  builtin_tokens.offset
                    rbnf_named__check_3 =  rbnf_named_parse_SHARING(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_named__check_4 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                                               builtin_tokens)
                        if builtin_eq(rbnf_named__check_4[0], False)
                            rbnf_named__check_4
                        else
                            rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                            rbnf_named__off_2 =  builtin_tokens.offset
                            if builtin_peekable(builtin_tokens, 0)
                                @switch  builtin_peek(builtin_tokens,
                                         0).idint begin
                                @case 20
                                  rbnf_named__check_5 =  rbnf_named_parse_rbnfmacro_9(builtin_state,
                                                         builtin_tokens)
                                  if builtin_eq(rbnf_named__check_5[0], False)
                                      rbnf_named__check_5
                                  else
                                      rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                      rbnf_tmp_2_ =  (( rbnf_tmp_3
                                                     , rbnf_tmp_4
                                                     , rbnf_tmp_5 ),)
                                      rbnf_tmp_3_ =  builtin_mk_ast("memoryDescriptor",
                                                     (( rbnf_tmp_0
                                                     , rbnf_tmp_1
                                                     , rbnf_tmp_2
                                                     , rbnf_tmp_1_
                                                     , rbnf_tmp_2_ ),))
                                      ((True, rbnf_tmp_3_),)
                                  end
                                @default
                                  rbnf_tmp_2_ =  ((),)
                                  rbnf_tmp_3_ =  (( rbnf_tmp_3
                                                 , rbnf_tmp_4
                                                 , rbnf_tmp_2_ ),)
                                  rbnf_tmp_4_ =  builtin_mk_ast("memoryDescriptor",
                                                 (( rbnf_tmp_0
                                                 , rbnf_tmp_1
                                                 , rbnf_tmp_2
                                                 , rbnf_tmp_1_
                                                 , rbnf_tmp_3_ ),))
                                  ((True, rbnf_tmp_4_),)
                                end
                            else
                                (( False
                                , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                              , "memoryDescriptor got EOF" ),),
                                                 builtin_nil)) ),)
                            end
                        end
                    end
                  @default
                    rbnf_tmp_1_ =  ((),)
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 19
                          rbnf_named__check_3 =  rbnf_named_parse_SHARING(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_named__check_4 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                                                     builtin_tokens)
                              if builtin_eq(rbnf_named__check_4[0], False)
                                  rbnf_named__check_4
                              else
                                  rbnf_tmp_4 =  builtin_to_result(rbnf_named__check_4[1])
                                  rbnf_named__off_2 =  builtin_tokens.offset
                                  if builtin_peekable(builtin_tokens, 0)
                                      @switch  builtin_peek(builtin_tokens,
                                               0).idint begin
                                      @case 20
                                        rbnf_named__check_5 =  rbnf_named_parse_rbnfmacro_9(builtin_state,
                                                               builtin_tokens)
                                        if builtin_eq(rbnf_named__check_5[0],
                                           False)
                                            rbnf_named__check_5
                                        else
                                            rbnf_tmp_5 =  builtin_to_result(rbnf_named__check_5[1])
                                            rbnf_tmp_2_ =  (( rbnf_tmp_3
                                                           , rbnf_tmp_4
                                                           , rbnf_tmp_5 ),)
                                            rbnf_tmp_3_ =  builtin_mk_ast("memoryDescriptor",
                                                           (( rbnf_tmp_0
                                                           , rbnf_tmp_1
                                                           , rbnf_tmp_2
                                                           , rbnf_tmp_1_
                                                           , rbnf_tmp_2_ ),))
                                            ((True, rbnf_tmp_3_),)
                                        end
                                      @default
                                        rbnf_tmp_2_ =  ((),)
                                        rbnf_tmp_3_ =  (( rbnf_tmp_3
                                                       , rbnf_tmp_4
                                                       , rbnf_tmp_2_ ),)
                                        rbnf_tmp_4_ =  builtin_mk_ast("memoryDescriptor",
                                                       (( rbnf_tmp_0
                                                       , rbnf_tmp_1
                                                       , rbnf_tmp_2
                                                       , rbnf_tmp_1_
                                                       , rbnf_tmp_3_ ),))
                                        ((True, rbnf_tmp_4_),)
                                      end
                                  else
                                      (( False
                                      , builtin_to_any(builtin_cons((( rbnf_named__off_2
                                                                    , "memoryDescriptor got EOF" ),),
                                                       builtin_nil)) ),)
                                  end
                              end
                          end
                        @default
                          rbnf_tmp_2_ =  ((),)
                          rbnf_tmp_3_ =  builtin_mk_ast("memoryDescriptor",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2
                                         , rbnf_tmp_1_
                                         , rbnf_tmp_2_ ),))
                          ((True, rbnf_tmp_3_),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "memoryDescriptor got EOF" ),),
                                         builtin_nil)) ),)
                    end
                  end
              else
                  (( False
                  , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                                , "memoryDescriptor got EOF" ),),
                                   builtin_nil)) ),)
              end
          end
      end
  end
end
function rbnf_named_parse_modifier(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 56
        rbnf_named__check_0 =  rbnf_named_parse_DAGGER(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("modifier", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 55
        rbnf_named__check_0 =  rbnf_named_parse_CONTROLLED(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("modifier", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "modifier lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "modifier got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_move(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_MOVE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_addr(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__off_0 =  builtin_tokens.offset
          if builtin_peekable(builtin_tokens, 0)
              @switch  builtin_peek(builtin_tokens, 0).idint begin
              @case 43
                rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 44
                rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 63
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 3
                rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 58
                rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 57
                rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @case 59
                rbnf_named__check_2 =  rbnf_named_parse_number(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_1_ =  builtin_mk_ast("move",
                                   ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                    ((True, rbnf_tmp_1_),)
                end
              @default
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                              , "move lookahead failed" ),),
                                 builtin_nil)) ),)
              end
          else
              (( False
              , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                            , "move got EOF" ),),
                               builtin_nil)) ),)
          end
      end
  end
end
function rbnf_named_parse_name(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("name", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_nonPowerExp(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 43
        rbnf_named__check_0 =  rbnf_named_parse_number(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 44
        rbnf_named__check_0 =  rbnf_named_parse_number(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 63
        rbnf_named__check_0 =  rbnf_named_parse_addr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 47
        rbnf_named__check_0 =  rbnf_named_parse_function(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_LPAREN(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_RPAREN(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                end
            end
        end
      @case 45
        rbnf_named__check_0 =  rbnf_named_parse_function(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_LPAREN(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_RPAREN(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                end
            end
        end
      @case 48
        rbnf_named__check_0 =  rbnf_named_parse_function(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_LPAREN(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_RPAREN(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                end
            end
        end
      @case 46
        rbnf_named__check_0 =  rbnf_named_parse_function(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_LPAREN(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_RPAREN(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                end
            end
        end
      @case 49
        rbnf_named__check_0 =  rbnf_named_parse_function(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__check_1 =  rbnf_named_parse_LPAREN(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_named__check_2 =  rbnf_named_parse_expression(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__check_3 =  rbnf_named_parse_RPAREN(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                end
            end
        end
      @case 3
        rbnf_named__off_1 =  builtin_tokens.offset
        rbnf_tmp_0 =  builtin_mv_forward(builtin_tokens)
        rbnf_named__check_1 =  rbnf_named_parse_expression(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                           ((rbnf_tmp_0, rbnf_tmp_1),))
            ((True, rbnf_tmp_1_),)
        end
      @case 2
        rbnf_tmp_0 =  builtin_mv_forward(builtin_tokens)
        rbnf_named__check_1 =  rbnf_named_parse_expression(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                           ((rbnf_tmp_0, rbnf_tmp_1),))
            ((True, rbnf_tmp_1_),)
        end
      @case 0
        rbnf_tmp_0 =  builtin_mv_forward(builtin_tokens)
        rbnf_named__check_1 =  rbnf_named_parse_expression(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_1[0], False)
            rbnf_named__check_1
        else
            rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
            rbnf_tmp_2 =  builtin_match_tk(builtin_tokens, 1)
            if builtin_is_null(rbnf_tmp_2)
                (( False
                , builtin_to_any(builtin_cons((( builtin_tokens.offset
                                              , "quote ) not match" ),),
                                 builtin_nil)) ),)
            else
                rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp",
                               ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
                ((True, rbnf_tmp_1_),)
            end
        end
      @case 66
        rbnf_named__check_0 =  rbnf_named_parse_variable(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 58
        rbnf_named__check_0 =  rbnf_named_parse_number(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 57
        rbnf_named__check_0 =  rbnf_named_parse_addr(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 59
        rbnf_named__check_0 =  rbnf_named_parse_number(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("nonPowerExp", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "nonPowerExp lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "nonPowerExp got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_nop(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_NOP(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("nop", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_number(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 43
        rbnf_tmp_1_ =  ((),)
        rbnf_named__off_1 =  builtin_tokens.offset
        rbnf_named__check_0 =  rbnf_named_parse_PI(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_2_ =  builtin_mk_ast("number",
                           ((rbnf_tmp_1_, rbnf_tmp_0),))
            ((True, rbnf_tmp_2_),)
        end
      @case 44
        rbnf_tmp_1_ =  ((),)
        rbnf_named__off_1 =  builtin_tokens.offset
        rbnf_named__check_0 =  rbnf_named_parse_I(builtin_state, builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_2_ =  builtin_mk_ast("number",
                           ((rbnf_tmp_1_, rbnf_tmp_0),))
            ((True, rbnf_tmp_2_),)
        end
      @case 3
        rbnf_named__check_0 =  rbnf_named_parse_MINUS(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_named__off_1 =  builtin_tokens.offset
            if builtin_peekable(builtin_tokens, 0)
                @switch  builtin_peek(builtin_tokens, 0).idint begin
                @case 43
                  rbnf_named__check_1 =  rbnf_named_parse_PI(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_1_ =  builtin_mk_ast("number",
                                     ((rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_1_),)
                  end
                @case 44
                  rbnf_named__check_1 =  rbnf_named_parse_I(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_1_ =  builtin_mk_ast("number",
                                     ((rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_1_),)
                  end
                @case 58
                  rbnf_named__check_1 =  rbnf_named_parse_imaginaryN(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_1_ =  builtin_mk_ast("number",
                                     ((rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_1_),)
                  end
                @case 59
                  rbnf_named__check_1 =  rbnf_named_parse_imaginaryN(builtin_state,
                                         builtin_tokens)
                  if builtin_eq(rbnf_named__check_1[0], False)
                      rbnf_named__check_1
                  else
                      rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                      rbnf_tmp_1_ =  builtin_mk_ast("number",
                                     ((rbnf_tmp_0, rbnf_tmp_1),))
                      ((True, rbnf_tmp_1_),)
                  end
                @default
                  (( False
                  , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                , "number lookahead failed" ),),
                                   builtin_nil)) ),)
                end
            else
                (( False
                , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                              , "number got EOF" ),),
                                 builtin_nil)) ),)
            end
        end
      @case 58
        rbnf_tmp_1_ =  ((),)
        rbnf_named__off_1 =  builtin_tokens.offset
        rbnf_named__check_0 =  rbnf_named_parse_imaginaryN(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_2_ =  builtin_mk_ast("number",
                           ((rbnf_tmp_1_, rbnf_tmp_0),))
            ((True, rbnf_tmp_2_),)
        end
      @case 59
        rbnf_tmp_1_ =  ((),)
        rbnf_named__off_1 =  builtin_tokens.offset
        rbnf_named__check_0 =  rbnf_named_parse_imaginaryN(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_2_ =  builtin_mk_ast("number",
                           ((rbnf_tmp_1_, rbnf_tmp_0),))
            ((True, rbnf_tmp_2_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "number lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "number got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_offsetDescriptor(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_OFFSET(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_INT(builtin_state, builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_tmp_1_ =  builtin_mk_ast("offsetDescriptor",
                             ((rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2),))
              ((True, rbnf_tmp_1_),)
          end
      end
  end
end
function rbnf_named_parse_param(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_expression(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("param", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_pragma(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_PRAGMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__off_0 =  builtin_tokens.offset
          if builtin_peekable(builtin_tokens, 0)
              @switch  builtin_peek(builtin_tokens, 0).idint begin
              @case 60
                rbnf_tmp_1_ =  ((),)
                rbnf_named__off_1 =  builtin_tokens.offset
                rbnf_named__check_2 =  rbnf_named_parse_STRING(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_tmp_2_ =  builtin_mk_ast("pragma",
                                   (( rbnf_tmp_0
                                   , rbnf_tmp_1
                                   , rbnf_tmp_1_
                                   , rbnf_tmp_2 ),))
                    ((True, rbnf_tmp_2_),)
                end
              @case 58
                rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_10(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 60
                          rbnf_named__check_3 =  rbnf_named_parse_STRING(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("pragma",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          rbnf_tmp_1_ =  ((),)
                          rbnf_tmp_2_ =  builtin_mk_ast("pragma",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2
                                         , rbnf_tmp_1_ ),))
                          ((True, rbnf_tmp_2_),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "pragma got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
              @case 57
                rbnf_named__check_2 =  rbnf_named_parse_rbnfmacro_10(builtin_state,
                                       builtin_tokens)
                if builtin_eq(rbnf_named__check_2[0], False)
                    rbnf_named__check_2
                else
                    rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                    rbnf_named__off_1 =  builtin_tokens.offset
                    if builtin_peekable(builtin_tokens, 0)
                        @switch  builtin_peek(builtin_tokens, 0).idint begin
                        @case 60
                          rbnf_named__check_3 =  rbnf_named_parse_STRING(builtin_state,
                                                 builtin_tokens)
                          if builtin_eq(rbnf_named__check_3[0], False)
                              rbnf_named__check_3
                          else
                              rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                              rbnf_tmp_1_ =  builtin_mk_ast("pragma",
                                             (( rbnf_tmp_0
                                             , rbnf_tmp_1
                                             , rbnf_tmp_2
                                             , rbnf_tmp_3 ),))
                              ((True, rbnf_tmp_1_),)
                          end
                        @default
                          rbnf_tmp_1_ =  ((),)
                          rbnf_tmp_2_ =  builtin_mk_ast("pragma",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_2
                                         , rbnf_tmp_1_ ),))
                          ((True, rbnf_tmp_2_),)
                        end
                    else
                        (( False
                        , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                      , "pragma got EOF" ),),
                                         builtin_nil)) ),)
                    end
                end
              @default
                rbnf_tmp_1_ =  ((),)
                rbnf_named__off_1 =  builtin_tokens.offset
                if builtin_peekable(builtin_tokens, 0)
                    @switch  builtin_peek(builtin_tokens, 0).idint begin
                    @case 60
                      rbnf_named__check_2 =  rbnf_named_parse_STRING(builtin_state,
                                             builtin_tokens)
                      if builtin_eq(rbnf_named__check_2[0], False)
                          rbnf_named__check_2
                      else
                          rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
                          rbnf_tmp_2_ =  builtin_mk_ast("pragma",
                                         (( rbnf_tmp_0
                                         , rbnf_tmp_1
                                         , rbnf_tmp_1_
                                         , rbnf_tmp_2 ),))
                          ((True, rbnf_tmp_2_),)
                      end
                    @default
                      rbnf_tmp_2_ =  ((),)
                      rbnf_tmp_3_ =  builtin_mk_ast("pragma",
                                     (( rbnf_tmp_0
                                     , rbnf_tmp_1
                                     , rbnf_tmp_1_
                                     , rbnf_tmp_2_ ),))
                      ((True, rbnf_tmp_3_),)
                    end
                else
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_1
                                                  , "pragma got EOF" ),),
                                     builtin_nil)) ),)
                end
              end
          else
              (( False
              , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                            , "pragma got EOF" ),),
                               builtin_nil)) ),)
          end
      end
  end
end
function rbnf_named_parse_pragma_name(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 58
        rbnf_named__check_0 =  rbnf_named_parse_INT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("pragma_name", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 57
        rbnf_named__check_0 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("pragma_name", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "pragma_name lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                    , "pragma_name got EOF" ),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_qubit(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 58
        rbnf_named__check_0 =  rbnf_named_parse_INT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("qubit", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 57
        rbnf_named__check_0 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("qubit", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "qubit lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "qubit got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_qubitVariable(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("qubitVariable", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end
function rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_allInstr(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_modifier(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_10(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_pragma_name(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_10(rbnf_tmp_1_,
      builtin_state,
      builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_2(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_param(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
          rbnf_named_lr_loop_rbnfmacro_2(rbnf_tmp_1_,
          builtin_state,
          builtin_tokens)
      end
  end
end
function rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_qubit(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_3(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_4(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_variable(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
          rbnf_named_lr_loop_rbnfmacro_4(rbnf_tmp_1_,
          builtin_state,
          builtin_tokens)
      end
  end
end
function rbnf_named_parse_rbnfmacro_5(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_matrixRow(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_5(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_6(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_COMMA(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_expression(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
          rbnf_named_lr_loop_rbnfmacro_6(rbnf_tmp_1_,
          builtin_state,
          builtin_tokens)
      end
  end
end
function rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_qubitVariable(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_7(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_8(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_instr(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_rbnfmacro_9(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_offsetDescriptor(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_push_list(builtin_empty_list(), rbnf_tmp_0)
      rbnf_named_lr_loop_rbnfmacro_9(rbnf_tmp_1_, builtin_state, builtin_tokens)
  end
end
function rbnf_named_parse_realN(builtin_state, builtin_tokens)
  rbnf_named__off_0 =  builtin_tokens.offset
  if builtin_peekable(builtin_tokens, 0)
      @switch  builtin_peek(builtin_tokens, 0).idint begin
      @case 58
        rbnf_named__check_0 =  rbnf_named_parse_INT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("realN", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @case 59
        rbnf_named__check_0 =  rbnf_named_parse_FLOAT(builtin_state,
                               builtin_tokens)
        if builtin_eq(rbnf_named__check_0[0], False)
            rbnf_named__check_0
        else
            rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
            rbnf_tmp_1_ =  builtin_mk_ast("realN", ((rbnf_tmp_0),))
            ((True, rbnf_tmp_1_),)
        end
      @default
        (( False
        , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                      , "realN lookahead failed" ),),
                         builtin_nil)) ),)
      end
  else
      (( False
      , builtin_to_any(builtin_cons(((rbnf_named__off_0, "realN got EOF"),),
                       builtin_nil)) ),)
  end
end
function rbnf_named_parse_resetState(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_RESET(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__off_0 =  builtin_tokens.offset
      if builtin_peekable(builtin_tokens, 0)
          @switch  builtin_peek(builtin_tokens, 0).idint begin
          @case 58
            rbnf_named__check_1 =  rbnf_named_parse_qubit(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("resetState",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
          @case 57
            rbnf_named__check_1 =  rbnf_named_parse_qubit(builtin_state,
                                   builtin_tokens)
            if builtin_eq(rbnf_named__check_1[0], False)
                rbnf_named__check_1
            else
                rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
                rbnf_tmp_1_ =  builtin_mk_ast("resetState",
                               ((rbnf_tmp_0, rbnf_tmp_1),))
                ((True, rbnf_tmp_1_),)
            end
          @default
            rbnf_tmp_1_ =  ((),)
            rbnf_tmp_2_ =  builtin_mk_ast("resetState",
                           ((rbnf_tmp_0, rbnf_tmp_1_),))
            ((True, rbnf_tmp_2_),)
          end
      else
          (( False
          , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                        , "resetState got EOF" ),),
                           builtin_nil)) ),)
      end
  end
end
function rbnf_named_parse_store(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_STORE(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_named__check_2 =  rbnf_named_parse_addr(builtin_state,
                                 builtin_tokens)
          if builtin_eq(rbnf_named__check_2[0], False)
              rbnf_named__check_2
          else
              rbnf_tmp_2 =  builtin_to_result(rbnf_named__check_2[1])
              rbnf_named__off_0 =  builtin_tokens.offset
              if builtin_peekable(builtin_tokens, 0)
                  @switch  builtin_peek(builtin_tokens, 0).idint begin
                  @case 43
                    rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @case 44
                    rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @case 63
                    rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @case 3
                    rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @case 58
                    rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @case 57
                    rbnf_named__check_3 =  rbnf_named_parse_addr(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @case 59
                    rbnf_named__check_3 =  rbnf_named_parse_number(builtin_state,
                                           builtin_tokens)
                    if builtin_eq(rbnf_named__check_3[0], False)
                        rbnf_named__check_3
                    else
                        rbnf_tmp_3 =  builtin_to_result(rbnf_named__check_3[1])
                        rbnf_tmp_1_ =  builtin_mk_ast("store",
                                       (( rbnf_tmp_0
                                       , rbnf_tmp_1
                                       , rbnf_tmp_2
                                       , rbnf_tmp_3 ),))
                        ((True, rbnf_tmp_1_),)
                    end
                  @default
                    (( False
                    , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                                  , "store lookahead failed" ),),
                                     builtin_nil)) ),)
                  end
              else
                  (( False
                  , builtin_to_any(builtin_cons((( rbnf_named__off_0
                                                , "store got EOF" ),),
                                   builtin_nil)) ),)
              end
          end
      end
  end
end
function rbnf_named_parse_variable(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_PERCENTAGE(builtin_state,
                         builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_named__check_1 =  rbnf_named_parse_IDENTIFIER(builtin_state,
                             builtin_tokens)
      if builtin_eq(rbnf_named__check_1[0], False)
          rbnf_named__check_1
      else
          rbnf_tmp_1 =  builtin_to_result(rbnf_named__check_1[1])
          rbnf_tmp_1_ =  builtin_mk_ast("variable", ((rbnf_tmp_0, rbnf_tmp_1),))
          ((True, rbnf_tmp_1_),)
      end
  end
end
function rbnf_named_parse_wait(builtin_state, builtin_tokens)
  rbnf_named__check_0 =  rbnf_named_parse_WAIT(builtin_state, builtin_tokens)
  if builtin_eq(rbnf_named__check_0[0], False)
      rbnf_named__check_0
  else
      rbnf_tmp_0 =  builtin_to_result(rbnf_named__check_0[1])
      rbnf_tmp_1_ =  builtin_mk_ast("wait", ((rbnf_tmp_0),))
      ((True, rbnf_tmp_1_),)
  end
end