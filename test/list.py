# This file is automatically generated by RBNF.hs
def rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
    #
    #newtmp: lcl_0
    lcl_0 = builtin_match_tk ( builtin_tokens, 8 )
    #release lcl_0
    rbnf_tmp_1 = lcl_0
    #lcl_0
    #reuse: lcl_0
    lcl_0 = rbnf_tmp_1 is None
    #release lcl_0
    #lcl_0
    #reuse: lcl_0
    if lcl_0:
        lcl_0 = builtin_null
    else:
        #
        #newtmp: lcl_1
        lcl_1 = append ( rbnf_tmp_0, rbnf_tmp_1 )
        #release lcl_1
        rbnf_tmp_1_ = lcl_1
        lcl_0 = rbnf_tmp_1_
    return lcl_0
def rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
    rbnf_named_lr_rbnfmacro_0_reduce = rbnf_tmp_0
    #
    #newtmp: lcl_0
    lcl_0 = builtin_tokens . offset
    #release lcl_0
    rbnf_named__off_0 = lcl_0
    #lcl_0
    #reuse: lcl_0
    lcl_0 = rbnf_named_lr_step_rbnfmacro_0 ( rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens )
    #release lcl_0
    rbnf_named_lr_rbnfmacro_0_try = lcl_0
    #lcl_0
    #reuse: lcl_0
    lcl_0 = rbnf_named_lr_rbnfmacro_0_try is not None
    while lcl_0:
        #
        #newtmp: lcl_1
        lcl_1 = builtin_tokens . offset
        #release lcl_1
        rbnf_named__off_0 = lcl_1
        rbnf_named_lr_rbnfmacro_0_reduce = rbnf_named_lr_rbnfmacro_0_try
        #lcl_1
        #reuse: lcl_1
        lcl_1 = rbnf_named_lr_step_rbnfmacro_0 ( rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens )
        #release lcl_1
        rbnf_named_lr_rbnfmacro_0_try = lcl_1
        # recalculate condition
        #lcl_1
        #reuse: lcl_1
        lcl_1 = rbnf_named_lr_rbnfmacro_0_try is not None
        #release lcl_1
        lcl_0 = lcl_1
    #release lcl_0
    #lcl_0lcl_1
    #reuse: lcl_0
    lcl_0 = builtin_tokens . offset
    #release lcl_0
    #lcl_0lcl_1
    #reuse: lcl_0
    lcl_0 = lcl_0 == rbnf_named__off_0
    #release lcl_0
    #lcl_0lcl_1
    #reuse: lcl_0
    if lcl_0:
        #lcl_1
        #reuse: lcl_1
        lcl_1 = ( True, rbnf_named_lr_rbnfmacro_0_reduce  )
        #release lcl_1
        lcl_0 = lcl_1
    else:
        lcl_0 = rbnf_named_lr_rbnfmacro_0_try
    return lcl_0
def rbnf_named_parse_START(builtin_state, builtin_tokens):
    #
    #newtmp: lcl_0
    lcl_0 = builtin_match_tk ( builtin_tokens, 2 )
    #release lcl_0
    rbnf_tmp_0 = lcl_0
    #lcl_0
    #reuse: lcl_0
    lcl_0 = rbnf_tmp_0 is None
    #release lcl_0
    #lcl_0
    #reuse: lcl_0
    if lcl_0:
        lcl_0 = builtin_null
    else:
        #
        #newtmp: lcl_1
        lcl_1 = rbnf_named_parse_g ( builtin_state, builtin_tokens )
        #release lcl_1
        rbnf_named__check_1 = lcl_1
        #lcl_1
        #reuse: lcl_1
        lcl_1 = rbnf_named__check_1 is None
        #release lcl_1
        #lcl_1
        #reuse: lcl_1
        if lcl_1:
            lcl_1 = builtin_null
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            #
            #newtmp: lcl_2
            lcl_2 = builtin_match_tk ( builtin_tokens, 3 )
            #release lcl_2
            rbnf_tmp_2 = lcl_2
            #lcl_2
            #reuse: lcl_2
            lcl_2 = rbnf_tmp_2 is None
            #release lcl_2
            #lcl_2
            #reuse: lcl_2
            if lcl_2:
                lcl_2 = builtin_null
            else:
                #
                #newtmp: lcl_3
                lcl_3 = ( rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2  )
                #release lcl_3
                #lcl_3
                #reuse: lcl_3
                lcl_3 = builtin_mk_ast ( "START", lcl_3 )
                #release lcl_3
                rbnf_tmp_1_ = lcl_3
                lcl_2 = rbnf_tmp_1_
            #release lcl_2
            lcl_1 = lcl_2
        #release lcl_1
        lcl_0 = lcl_1
    return lcl_0
def rbnf_named_parse_g(builtin_state, builtin_tokens):
    #
    #newtmp: lcl_0
    lcl_0 = builtin_tokens . offset
    #release lcl_0
    rbnf_named__off_0 = lcl_0
    #lcl_0
    #reuse: lcl_0
    lcl_0 = builtin_peekable ( builtin_tokens, 0 )
    #release lcl_0
    #lcl_0
    #reuse: lcl_0
    if lcl_0:
        # switch
        #
        #newtmp: lcl_1
        #
        #newtmp: lcl_2
        lcl_2 = builtin_peek ( builtin_tokens, 0 )
        #release lcl_2
        #lcl_2
        #reuse: lcl_2
        lcl_2 = lcl_2 . idint
        if lcl_2 == 8:
            #
            #newtmp: lcl_3
            lcl_3 = rbnf_named_parse_rbnfmacro_0 ( builtin_state, builtin_tokens )
            #release lcl_3
            rbnf_named__check_0 = lcl_3
            #lcl_3
            #reuse: lcl_3
            lcl_3 = rbnf_named__check_0 is None
            #release lcl_3
            #lcl_3
            #reuse: lcl_3
            if lcl_3:
                lcl_3 = builtin_null
            else:
                rbnf_tmp_0 = rbnf_named__check_0
                #
                #newtmp: lcl_4
                lcl_4 = ( rbnf_tmp_0 , )
                #release lcl_4
                #lcl_4
                #reuse: lcl_4
                lcl_4 = builtin_mk_ast ( "g", lcl_4 )
                #release lcl_4
                rbnf_tmp_1_ = lcl_4
                lcl_3 = rbnf_tmp_1_
            #release lcl_3
            lcl_1 = lcl_3
        elif lcl_2 == 7:
            #lcl_3lcl_4
            #reuse: lcl_3
            lcl_3 = builtin_mv_forward ( builtin_tokens )
            #release lcl_3
            rbnf_tmp_0 = lcl_3
            #lcl_3lcl_4
            #reuse: lcl_3
            lcl_3 = ( rbnf_tmp_0 , )
            #release lcl_3
            #lcl_3lcl_4
            #reuse: lcl_3
            lcl_3 = builtin_mk_ast ( "g", lcl_3 )
            #release lcl_3
            rbnf_tmp_1_ = lcl_3
            lcl_1 = rbnf_tmp_1_
        else:
            lcl_1 = builtin_null
        #release lcl_2
        #release lcl_1
        lcl_0 = lcl_1
    else:
        lcl_0 = builtin_null
    return lcl_0
def rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens):
    #
    #newtmp: lcl_0
    lcl_0 = builtin_match_tk ( builtin_tokens, 8 )
    #release lcl_0
    rbnf_tmp_0 = lcl_0
    #lcl_0
    #reuse: lcl_0
    lcl_0 = rbnf_tmp_0 is None
    #release lcl_0
    #lcl_0
    #reuse: lcl_0
    if lcl_0:
        lcl_0 = builtin_null
    else:
        #
        #newtmp: lcl_1
        lcl_1 = builtin_empty_list (  )
        #release lcl_1
        #lcl_1
        #reuse: lcl_1
        lcl_1 = builtin_push_list ( lcl_1, rbnf_tmp_0 )
        #release lcl_1
        rbnf_tmp_1_ = lcl_1
        #lcl_1
        #reuse: lcl_1
        lcl_1 = rbnf_named_lr_loop_rbnfmacro_0 ( rbnf_tmp_1_, builtin_state, builtin_tokens )
        #release lcl_1
        lcl_0 = lcl_1
    return lcl_0
