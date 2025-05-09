typing_fail = failed to type {$phase}
    .note = see errors above

typing_occurck_failed = unsolvable constraint: `{$var} == {$ty}`
    .note = of this expression

typing_general_expect = type mismatch
    .label = {$expr_kind} has type `{$found_ty}` but an expression was expected of type `{$expected_ty}`

typing_unify_then_else_different = `then` and `else` have incompatible types
    .label = `then` and `else` have incompatible types
    .reason = expected because of this

typing_unbound_ident = unbound identifier
    .label = unbound variable: `{$var}`

typing_invalid_set_syntax = invalid syntax of `<-`
    .label = left hand side of `<-` must be a form of `base.(index)`
    .bug_report = this might be a bug, consult TAs or developers

typing_invalid_type_ascription = invalid type ascription
    .note = take a look at the `.mli` files and try another syntax
    .bug_report = this might be a bug, consult TAs or developers

typing_typevar_remain = unresolved type variables
    .label = type of this expression remains unresolved
