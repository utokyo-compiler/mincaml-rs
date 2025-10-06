//! This module contains implementations of the EDSL for the [`decide_ty`] method
//! of the [`TypeChecker`] struct. Therefore it contains no items
//! since items attributed `#[macro_export]` belongs to the crate
//! instead of modules.
//!
//! [`decide_ty`]: crate::typeck::TypeChecker::decide_ty
//! [`TypeChecker`]: crate::typeck::TypeChecker

#[macro_export]
/// An embedded DSL for [`decide_ty`].
///
/// This macro is an abstraction for common patterns in [`decide_ty`] method.
/// As the name suggests, it is expected to be used in [`decide_ty`] method.
///
/// This macro is meant to extract the similarities in the code, so modifications
/// to the macro should be done on global changes (that of the entire structure
/// of [`decide_ty`] itself, or that of [`TypeChecker`] for example), with extreme care.
/// If you find yourself modifying the macro for a specific case,
/// **consider inlining the macro** and modifying the code directly.
///
/// # Syntax
///
/// The macro syntax in ABNF format is as follows:
///
/// ```ignore
/// PassSelfKw = "self" ";"
/// IdentifierList = RustIdentifier ("," RustIdentifier)*
/// QuoteOCamlTy = "unit" / "bool" / "int" / "float"
/// QuoteOCamlTy =/ "#" RustIdentifier "->" "#" RustIdentifier
/// QuoteOCamlTy =/ "#" RustIdentifier "array"
/// QuoteOCamlTy =/ "#" RustIdentifier
/// QuoteOCamlTy =/ "(" QuoteOCamlTy ")"
/// QuoteOCamlTy =/ "{" QuoteOCamlTy "}"
///
/// PairTySpec = "(" "'a" "," "'a" ")"; `('a, 'a)`
/// PairTySpec =/ "(" "'a" "array" "," "'a" ")"; `('a array, 'a)`
///
/// InterludeContent = RustIdentifier ":" QuoteOCamlTy ";"
/// InterludeContent =/
///     "(" RustIdentifier "," RustIdentifier ")" ":" PairTySpec ";"
///
/// RecoverEnd =  "return" "recover" RustExpression
/// RecoverEnd =/ "return" "fail"
/// RecoverBody = (RustStmt ";")* RecoverEnd ";"
/// PatternGuard =  "if" "let" RustPattern "=" RustExpression "=>"
/// PatternGuard =/ |IdentifierList|
/// PatternGuard =/ ; empty
/// RecoverBranch = "else"? PatternGuard "{" RecoverBody "}"
///
/// Prelude = "decide" IdentifierList
/// Interlude = "unify" "{" InterludeContent* "}"
/// Postlude =  "("
///     "|" IdentifierList "|" QuoteOCamlTy
///         ("on" "recover" "{" (RustStmt ";")* "}")?
///         ("else" "{" RecoverBranch* "}")? ","
///     RustExpression ","?
/// ")"
/// Postlude =/ "(" "|" "|" QuoteOCamlTy "," RustExpression ","? ")"
///
/// MacroContent =
///     Prelude ";"
///     Interlude
///     Postlude
///
/// MacroContent =/
///     "prelude:" Prelude ";"?
///
/// MacroContent =/
///     "interlude:" Interlude
///
/// MacroContent =/
///     "postlude:" "for" "(" RustIdentifierList ")" Postlude
///
/// Start = "decide_ty_edsl!" "{" PassSelfKw
///     MacroContent
/// "}"
/// ```
///
/// Note that this syntax slightly differs from the actual macro syntax.
///
/// # Implementation
///
/// This macro takes `self` variable as `$the_self:ident` because of hygiene.
///
/// [`decide_ty`]: crate::typeck::TypeChecker::decide_ty
/// [`TypeChecker`]: crate::typeck::TypeChecker
macro_rules! decide_ty_edsl {
    ($the_self:ident; prelude: decide $($ident:ident),+ $(;)?) => {
        $(
            let $ident = $the_self.decide_ty($ident);
        )*
    };
    ($the_self:ident; interlude: unify {$($tt:tt)*}) => {
        $crate::decide_ty_edsl_interlude_impl!([$the_self] $($tt)*)
    };
    ($the_self:ident; postlude: for($($ident:ident),+) $($tt:tt)*) => {
        $crate::decide_ty_edsl_postlude_impl!([$the_self] [$($ident),+] [($($ident),+)] $($tt)*)
    };
    // public API: combine all three parts
    (
        $the_self:ident;
        decide $($ident:ident),+;
        unify {$($interlude_tt:tt)*}
        $($postlude_tt:tt)*
    ) => {
        $crate::decide_ty_edsl! { $the_self; prelude: decide $($ident),+; }
        $crate::decide_ty_edsl! { $the_self; interlude: unify {$($interlude_tt)*} }
        $crate::decide_ty_edsl! { $the_self; postlude: for($($ident),+) $($postlude_tt)* }
    };
}

#[macro_export]
#[doc(hidden)]
/// Internal macro to implement the unification part of the [`decide_ty_edsl!`] macro.
///
/// This is a TT muncher, so its branches should be kept small.
///
/// This macro takes `[$the_self:ident]` because of hygiene.
macro_rules! decide_ty_edsl_interlude_impl {
    // Base case of TT muncher: no tokens left
    ([$the_self:ident]) => {};
    // example: `[self] e1: bool;` in `If(e1, e2, e3)`
    ([$the_self:ident] $ident:ident: $ocaml_ty:tt $(else { $($el:tt)* })?; $($rest:tt)*) => {
        if let Some($ident) = $ident.ty() {
            $the_self.unify_with_current_subst(
                $ident,
                $crate::quote_ocaml_ty!([$the_self] $ocaml_ty)
            ).unwrap();
        } $(else { $($el)* })?
        $crate::decide_ty_edsl_interlude_impl!([$the_self] $($rest)*)
    };
    // example: `[self] (e2, e3): ('a, 'a)` in `If(e1, e2, e3)`
    ([$the_self:ident] ($ident1:ident, $ident2:ident): ('a, 'a) $(else { $($el:tt)* })?; $($rest:tt)*) => {
        if let (Some($ident1), Some($ident2)) = ($ident1.ty(), $ident2.ty()) {
            $the_self.unify_with_current_subst(
                $ident1,
                $ident2
            ).unwrap();
        } $(else { $($el)* })?
    };
    // example: `[self] (e1, e3): ('a array, 'a)` in `Set(Get(e1, e2), e3)`
    ([$the_self:ident] ($ident1:ident, $ident2:ident): ('a array, 'a) $(else { $($el:tt)* })?; $($rest:tt)*) => {
        if let (Some($ident1), Some($ident2)) = ($ident1.ty(), $ident2.ty()) {
            $the_self.unify_with_current_subst(
                $ident1,
                $crate::quote_ocaml_ty!{
                    [$the_self] (#$ident2 array)
                }
            ).unwrap();
        } $(else { $($el)* })?
    };
}

#[macro_export]
#[doc(hidden)]
/// Internal macro to implement the Ocaml type quotation in the [`decide_ty_edsl!`] macro.
///
/// This macro takes `[$the_self:ident]` because of hygiene.
/// This macro uses [`ty::HasTy`] to get the type of given identifiers.
macro_rules! quote_ocaml_ty {
    ([$the_self:ident] unit) => {
        $the_self.common_types.unit
    };
    ([$the_self:ident] bool) => {
        $the_self.common_types.bool
    };
    ([$the_self:ident] int) => {
        $the_self.common_types.int
    };
    ([$the_self:ident] float) => {
        $the_self.common_types.float
    };
    ([$the_self:ident] #$ident1:ident -> #$ident2:ident) => {
        Ty::mk_fun($the_self.ctx, $ident1, $ident2.ty())
    };
    ([$the_self:ident] #$ident:ident array) => {
        Ty::mk_array($the_self.ctx, $ident.ty())
    };
    ([$the_self:ident] #$ident:ident) => {
        $ident.ty()
    };

    // These two rules require parentheses, since this macro accepts a single TT.
    ([$the_self:ident] ($($tt:tt)*)) => {
        $crate::quote_ocaml_ty! { [$the_self] $($tt)* }
    };
    ([$the_self:ident] {$($tt:tt)*}) => {
        $crate::quote_ocaml_ty! { [$the_self] $($tt)* }
    };
}

#[macro_export]
#[doc(hidden)]
/// Internal macro to implement the last part of the [`decide_ty_edsl!`] macro.
///
/// This macro takes `[$the_self:ident]` because of hygiene.
macro_rules! decide_ty_edsl_postlude_impl {
    // The return type depends on the result of the identifiers.
    // That is because recover branches are required to write.
    //
    // The argument `[$($ident:ident),+]` and `[$pat:pat]` are similar to each other,
    // but the latter is free from expansion. See their use site for more details.
    //
    // example: `[self] [e1, e2, e3] [(e1, e2, e3)] (|e2| (#e2), If(e1, e2, e3)) else {
    //     |e3| {
    //         return recover (#e3);
    //     }
    // }` in `If(e1, e2, e3)`
    ([$the_self:ident] [$($ident:ident),+] [$pat:pat] (
        |$($pat_ident_normal:ident),+| $ocaml_ty:tt $(on recover {
            $($recover_branch:tt)+
        })? $(else {
            $(
                $(else)? $(|$($pat_ident:ident),*|)? $(if let $gpat:pat = $gexpr:expr =>)? {$($recover_branch_other:tt)+}
            )+
        })?,
        $parsed:expr $(,)?
    )) => {
        match ($($ident),*) {
            ($(ResultWithRecover::Ok($ident)),*) => {
                ($crate::quote_ocaml_ty!([$the_self] $ocaml_ty), $parsed)
            }
            #[allow(unused_parens)]
            #[allow(unused_variables)]
            $pat if let ($(Some($pat_ident_normal)),*) = ($($pat_ident_normal.ty()),*)
            => {
                $crate::decide_ty_edsl_postlude_impl! {
                    [$the_self] [$ocaml_ty] @branch {
                        $($($recover_branch:tt)+)?
                        return recover $ocaml_ty;
                    }
                }
            }
            $($(
                #[allow(unused_parens)]
                #[allow(unused_variables)]
                $pat $(if let $gpat = $gexpr)? $(if let ($(Some($pat_ident)),*) = ($($pat_ident.ty()),*))?
                => {
                    $crate::decide_ty_edsl_postlude_impl! {
                        [$the_self] [$ocaml_ty] @branch {
                            $($recover_branch_other)*
                        }
                    }
                }
            )+)?
            _ => {
                return DecideTyReturn::Fail(());
            }
        }
    };
    ([$the_self:ident] [$ocaml_ty:tt] @branch {
        return recover;
    }) => {
        return DecideTyReturn::Recover(
            $crate::quote_ocaml_ty!([$the_self] $ocaml_ty)
        );
    };
    ([$the_self:ident] [$ocaml_ty:tt] @branch {
        return recover $recover_ty:tt;
    }) => {
        return DecideTyReturn::Recover(
            $crate::quote_ocaml_ty!([$the_self] $recover_ty)
        )
    };
    ([$the_self:ident] [$ocaml_ty:tt] @branch {
        return fail;
    }) => {
        return DecideTyReturn::Fail(());
    };
    ([$the_self:ident] [$ocaml_ty:tt] @branch {
        $stmt:stmt;
        $($rest:tt)*
    }) => {
        $stmt
        $crate::decide_ty_edsl_postlude_impl! {
            [$the_self] [$ocaml_ty] @branch {
                $($rest)*
            }
        }
    };
    ([$the_self:ident] [$ocaml_ty:tt] @branch {}) => {};
    // The return type is always known as `$ocaml_ty`.
    //
    // example: `[self] [e1, e2] [(e1, e2)] (|| int, Binary(Add, e1, e2))` in `Binary(Add, e1, e2)`
    ([$the_self:ident] [$($ident:ident),+] [$pat:pat] (
        || $ocaml_ty:tt,
        $expr:expr $(,)?
    )) => {
        let return_type = $crate::quote_ocaml_ty!([$the_self] $ocaml_ty);
        match ($($ident),*) {
            #[allow(unused_parens)]
            ($(ResultWithRecover::Ok($ident)),*) => {
                (return_type, $expr)
            }
            _ => {
                return DecideTyReturn::Recover(return_type);
            }
        }
    };
}
