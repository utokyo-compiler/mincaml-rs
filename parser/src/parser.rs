use std::collections::VecDeque;

use sourcemap::{Loc, Spanned};
use syntax::Expr;

use crate::lexer::Token;

pub fn parse<'t>(tokens: &[Spanned<Token<'t>>]) -> Result<Expr<'t>, Error<'t>> {
    let parser = Parser::new(tokens);
    mincaml::exp(&parser, &parser).map_err(|e| {
        let span = parser.tokens[e.location].to_owned();
        Error::PegError(span, e.expected)
    })
}

#[derive(Debug, Clone)]
pub enum Error<'t> {
    PegError(Spanned<Token<'t>>, peg::error::ExpectedSet),
}

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
        macro_rules! match_tokens {
            ($expr:expr, $literal:ident { $($l:ident $r:literal)* }) => {
                match $expr {
                    $(
                        $l => match $literal {
                            $r => peg::RuleResult::Matched(pos + 1, ()),
                            _ => peg::RuleResult::Failed,
                        },
                    )*
                    _ => peg::RuleResult::Failed,
                }
            };
        }
        match_tokens!(&elem.node, literal {
            LPar "("
            RPar ")"
            Not "not"
            Hyphen "-"
            Plus "+"
            Ast "*"
            Slash "/"
            HyphenDot "-."
            PlusDot "+."
            AstDot "*."
            SlashDot "/."
            Equal "="
            LessGreater "<>"
            LessEqual "<="
            GreaterEqual ">="
            Less "<"
            Greater ">"
            If "if"
            Then "then"
            Else "else"
            Let "let"
            In "in"
            Rec "rec"
            Comma ","
            ArrayMake "Array.make"
            Dot "."
            LessHyphen "<-"
            Semi ";"
        })
    }
}

peg::parser! {
    pub grammar mincaml<'t, 'lexer>(parser_ref: &Parser<'t, 'lexer>) for Parser<'t, 'lexer> {
        use syntax::*;
        use syntax::ExprKind::*;
        use syntax::LitKind::*;
        use syntax::UnOp::*;
        use syntax::BinOp::*;
        use syntax::BBinOpKind::*;
        use syntax::IBinOpKind::*;
        use syntax::FBinOpKind::*;
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
        pub rule exp() -> Expr<'t>
            = l:l() "Array.make" e1:simple_exp() e2:simple_exp() {
                let span = (l, e2.span.end);
                Expr::new(ArrayMake(Box::new(e1), Box::new(e2)), span)
            }
            / l:l() "not" e:simple_exp() {
                let span = (l, e.span.end);
                Expr::new(Unary(Not, Box::new(e)), span)
            }
            / e:exp_infix() { e }
            / e1:simple_exp() "." "(" e2:exp() ")" "<-" e3:exp() {
                let span = (e1.span.start, e3.span.end);
                Expr::new(Set(Box::new(e1), Box::new(e2), Box::new(e3)), span)
            }
            / v:simple_exp()+ {
                let mut v = VecDeque::from_iter(v);
                let e1 = v.pop_front().unwrap();
                if !v.is_empty() {
                    let span = (e1.span.start, v.back().unwrap().span.end);
                    Expr::new(App(Box::new(e1), v.into()), span)
                }
                else {
                    e1
                }
            }
        rule exp_infix() -> Expr<'t> = precedence! {
            x:(@) r:op_rel() y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(BBinOp(r), Box::new(x), Box::new(y)), span)
            }
            --
            x:(@) "+" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Add), Box::new(x), Box::new(y)), span)
            }
            x:(@) "-" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Sub), Box::new(x), Box::new(y)), span)
            }
            x:(@) "+." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FAdd), Box::new(x), Box::new(y)), span)
            }
            x:(@) "-." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FSub), Box::new(x), Box::new(y)), span)
            }
            --
            x:(@) "*" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Mul), Box::new(x), Box::new(y)), span)
            }
            x:(@) "/" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Div), Box::new(x), Box::new(y)), span)
            }
            x:(@) "*." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FMul), Box::new(x), Box::new(y)), span)
            }
            x:(@) "/." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FDiv), Box::new(x), Box::new(y)), span)
            }
        }
        rule op_rel() -> BBinOpKind
            = "<>" { Ne }
            / "<=" { Le }
            / ">=" { Ge }
            / "<" { Lt }
            / ">" { Gt }
            / "=" { Eq }
        rule l() -> Loc<usize> = p:position!() {?
            if p < parser_ref.tokens.len() {
                Ok(parser_ref.tokens[p].span.start)
            } else {
                Err("")
            }
        }
        rule r() -> Loc<usize> = p:position!() { parser_ref.tokens[p - 1].span.end }
    }
}

#[cfg(test)]
mod tests {
    use syntax::{ExprKind, UnOp};

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

    #[test]
    fn test_exp() {
        let spanned = test_parser("a b c", mincaml::exp);
        assert!(matches!(spanned.node, ExprKind::App(_, _)));
        let spanned = test_parser("Array.make 1 1", mincaml::exp);
        assert!(matches!(spanned.node, ExprKind::ArrayMake(_, _)));
        let spanned = test_parser("not true", mincaml::exp);
        assert!(matches!(spanned.node, ExprKind::Unary(UnOp::Not, _)));
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
        // dbg!(&v);
        let p = Parser::new(&v);
        f(&p, &p).unwrap()
    }
}
