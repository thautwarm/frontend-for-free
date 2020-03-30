function var"rbnf_namedtmp_lr_step_rbnfmacro_0"(var"rbnf_tmp_0",
var"builtin_state",
var"builtin_tokens")
var"rbnf_tmp_1" =  var"builtin_match_tk"(var"builtin_tokens",
                   var"builtin_tk_id"("quote a"))
if var"builtin_is_null"(var"rbnf_tmp_1")
    (( False
    , var"builtin_to_any"(var"builtin_cons"((( var"builtin_tokens".offset
                                            , "quote a not match" ),),
                          var"builtin_nil")) ),)
else
    var"rbnf_tmp_1_" =  var"append"(var"rbnf_tmp_0", var"rbnf_tmp_1")
    ((True, var"rbnf_tmp_1_"),)
end
end
function var"rbnf_namedtmp_lr_loop_rbnfmacro_0"(var"rbnf_tmp_0",
var"builtin_state",
var"builtin_tokens")
var"rbnf_namedtmp_lr_rbnfmacro_0_reduce" =  var"rbnf_tmp_0"
var"rbnf_namedtmp__off_0" =  var"builtin_tokens".offset
var"rbnf_namedtmp_lr_rbnfmacro_0_try" =  var"rbnf_namedtmp_lr_step_rbnfmacro_0"(var"rbnf_namedtmp_lr_rbnfmacro_0_reduce",
                                         var"builtin_state",
                                         var"builtin_tokens")
while var"builtin_not_eq"(var"rbnf_namedtmp_lr_rbnfmacro_0_try"[0], False)
    var"rbnf_namedtmp__off_0" =  var"builtin_tokens".offset
    var"rbnf_namedtmp_lr_rbnfmacro_0_reduce" =  var"builtin_to_result"(var"rbnf_namedtmp_lr_rbnfmacro_0_try"[1])
    var"rbnf_namedtmp_lr_rbnfmacro_0_try" =  var"rbnf_namedtmp_lr_step_rbnfmacro_0"(var"rbnf_namedtmp_lr_rbnfmacro_0_reduce",
                                             var"builtin_state",
                                             var"builtin_tokens")
end
if var"builtin_eq"(var"builtin_tokens".offset, var"rbnf_namedtmp__off_0")
    ((True, var"rbnf_namedtmp_lr_rbnfmacro_0_reduce"),)
else
    var"rbnf_namedtmp_lr_rbnfmacro_0_try"
end
end
function var"rbnf_namedtmp_parse_START"(var"builtin_state", var"builtin_tokens")
var"rbnf_namedtmp__check_0" =  var"rbnf_namedtmp_parse_g"(var"builtin_state",
                               var"builtin_tokens")
if var"builtin_eq"(var"rbnf_namedtmp__check_0"[0], False)
    var"rbnf_namedtmp__check_0"
else
    var"rbnf_tmp_0" =  var"builtin_to_result"(var"rbnf_namedtmp__check_0"[1])
    var"rbnf_tmp_1_" =  var"builtin_mk_ast"("START", ((var"rbnf_tmp_0"),))
    ((True, var"rbnf_tmp_1_"),)
end
end
function var"rbnf_namedtmp_parse_g"(var"builtin_state", var"builtin_tokens")
var"rbnf_namedtmp__check_0" =  var"rbnf_namedtmp_parse_rbnfmacro_0"(var"builtin_state",
                               var"builtin_tokens")
if var"builtin_eq"(var"rbnf_namedtmp__check_0"[0], False)
    var"rbnf_namedtmp__check_0"
else
    var"rbnf_tmp_0" =  var"builtin_to_result"(var"rbnf_namedtmp__check_0"[1])
    var"rbnf_tmp_1_" =  var"builtin_mk_ast"("g", ((var"rbnf_tmp_0"),))
    ((True, var"rbnf_tmp_1_"),)
end
end
function var"rbnf_namedtmp_parse_rbnfmacro_0"(var"builtin_state",
var"builtin_tokens")
var"rbnf_tmp_0" =  var"builtin_match_tk"(var"builtin_tokens",
                   var"builtin_tk_id"("quote a"))
if var"builtin_is_null"(var"rbnf_tmp_0")
    (( False
    , var"builtin_to_any"(var"builtin_cons"((( var"builtin_tokens".offset
                                            , "quote a not match" ),),
                          var"builtin_nil")) ),)
else
    var"rbnf_tmp_1_" =  var"builtin_push_list"(var"builtin_empty_list",
                        var"rbnf_tmp_0")
    var"rbnf_namedtmp_lr_loop_rbnfmacro_0"(var"rbnf_tmp_1_",
    var"builtin_state",
    var"builtin_tokens")
end
end
