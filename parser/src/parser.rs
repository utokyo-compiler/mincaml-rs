use sourcemap::{Loc, Spanned};
use syntax::Expr;

use crate::lexer::Token;

struct Parser<'t, 'lexer> {
    tokens: &'lexer [Spanned<Token<'t>>],
}

impl<'t, 'lexer> Parser<'t, 'lexer> {
    fn new(tokens: &'lexer [Spanned<Token<'t>>]) -> Self {
        Self { tokens }
    }
}

#[allow(clippy::needless_lifetimes)]
impl<'t> peg::Parse for Parser<'t, '_> {
    type PositionRepr = <[Spanned<Token<'t>>] as peg::Parse>::PositionRepr;

    fn start<'input>(&'input self) -> usize {
        self.tokens.start()
    }

    fn is_eof<'input>(&'input self, p: usize) -> bool {
        self.tokens.is_eof(p)
    }

    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr {
        self.tokens.position_repr(p)
    }
}

impl<'input, 't: 'input> peg::ParseElem<'input> for Parser<'t, '_> {
    type Element = &'input Spanned<Token<'t>>;

    fn parse_elem(&'input self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self.tokens[pos..].first() {
            Some(c) => peg::RuleResult::Matched(pos + 1, c),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'t> peg::ParseLiteral for Parser<'t, '_> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        let Some(elem) = self.tokens[pos..].first() else {
            return peg::RuleResult::Failed;
        };
        use Token::*;
        match (&elem.node, literal) {
            (LPar, "(") | (RPar, ")") | (Dot, ".") => peg::RuleResult::Matched(pos + 1, ()),
            _ => peg::RuleResult::Failed,
        }
    }
}

peg::parser! {
    pub grammar mincaml<'t, 'lexer>(parser_ref: &Parser<'t, 'lexer>) for Parser<'t, 'lexer> {
        use syntax::ExprKind::*;
        use syntax::LitKind::*;
        /// 括弧をつけなくても関数の引数になれる式
        #[cache_left_rec]
        pub rule simple_exp() -> Expr<'t>
            = "(" e:exp() ")" { e }
            / l:l() "(" ")" r:r() { Expr::new(Lit(Unit), (l, r)) }
            / e1:simple_exp() "." "(" e2:exp() ")" r:r() {
                let span = (e1.span.start, r);
                Expr::new(Get(Box::new(e1), Box::new(e2)), span)
            }
            / [Spanned { node, span }] {?
                Ok(Spanned {
                    node: match node {
                        Token::Int(i) => Lit(Int(*i)),
                        Token::Bool(b) => Lit(Bool(*b)),
                        Token::Float(f) => Lit(Float(*f)),
                        Token::Ident(x) => Var(x),
                        _ => Err("")?,
                    },
                    span: span.clone(),
                })
            }
        /// 一般の式
        pub rule exp() -> Expr<'t> = precedence!{
            e:simple_exp() { e }
        }
        rule l() -> Loc<usize> = p:position!() { parser_ref.tokens[p].span.start }
        rule r() -> Loc<usize> = p:position!() { parser_ref.tokens[p - 1].span.end }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_simple_exp() {
        assert_eq!(
            test_parser("((*  *)()     )", mincaml::simple_exp).range(),
            7..9
        );
        test_parser("a", mincaml::simple_exp);
        test_parser("a.(0)", mincaml::simple_exp);
        test_parser("a.(0).(0)", mincaml::simple_exp);
    }

    type PegRule<'a> = for<'lexer> fn(
        &Parser<'a, 'lexer>,
        &Parser<'a, 'lexer>,
    ) -> Result<
        Spanned<syntax::ExprKind<'a>>,
        peg::error::ParseError<<Parser<'a, 'lexer> as peg::Parse>::PositionRepr>,
    >;
    fn test_parser<'a>(input: &'a str, f: PegRule<'a>) -> Spanned<syntax::ExprKind<'a>> {
        let v = Lexer::new(input).read_to_vec().unwrap();
        let p = Parser::new(&v);
        f(&p, &p).unwrap()
    }
}
