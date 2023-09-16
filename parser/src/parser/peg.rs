use std::collections::VecDeque;

use bumpalo::Bump;
use sourcemap::{Loc, Spanned};
use syntax::Expr;

use crate::{
    lexer::{Lexer, Token},
    parser::{Error, ParseError, Parser},
};

use super::ExpectedTokens;

pub struct PegParser;

impl Parser for PegParser {
    fn parse<'t, 'b: 't>(bump: &'b Bump, lexer: impl Lexer<'t>) -> Result<Expr<'t, 'b>, Error<'t>> {
        let tokens = lexer.read_to_vec().map_err(Error::LexError)?;
        let parser = PegParserImpl::new(&tokens);
        mincaml::exp(&parser, bump, &parser).map_err(|e| {
            let span = parser.tokens[e.location].to_owned();
            Error::ParseError(ParseError::UnexpectedToken(
                span,
                ExpectedTokens(e.expected.tokens().map(|s| s.to_string()).collect()),
            ))
        })
    }
}

struct PegParserImpl<'t, 'lexer> {
    tokens: &'lexer [Spanned<Token<'t>>],
}

impl<'t, 'lexer> PegParserImpl<'t, 'lexer> {
    fn new(tokens: &'lexer [Spanned<Token<'t>>]) -> Self {
        Self { tokens }
    }
}

#[allow(clippy::needless_lifetimes)]
impl<'t> peg::Parse for PegParserImpl<'t, '_> {
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

impl<'input, 't: 'input> peg::ParseElem<'input> for PegParserImpl<'t, '_> {
    type Element = &'input Spanned<Token<'t>>;

    fn parse_elem(&'input self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self.tokens[pos..].first() {
            Some(c) => peg::RuleResult::Matched(pos + 1, c),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'t> peg::ParseLiteral for PegParserImpl<'t, '_> {
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
    pub grammar mincaml<'t, 'b, 'lexer>(bump: &'b Bump, parser_ref: &PegParserImpl<'t, 'lexer>) for PegParserImpl<'t, 'lexer> {
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
        pub rule simple_exp() -> Expr<'t, 'b>
            = "(" e:exp() ")" { e }
            / l:l() "(" ")" r:r() { Expr::new(Lit(Unit), (l, r)) }
            / e1:simple_exp() "." "(" e2:exp() ")" r:r() {
                let span = (e1.span.start, r);
                Expr::new(Get(bump.alloc(e1), bump.alloc(e2)), span)
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
        pub rule exp() -> Expr<'t, 'b>
            = l:l() "Array.make" e1:simple_exp() e2:simple_exp() {
                let span = (l, e2.span.end);
                Expr::new(ArrayMake(bump.alloc(e1), bump.alloc(e2)), span)
            }
            / l:l() "not" e:simple_exp() {
                let span = (l, e.span.end);
                Expr::new(Unary(Not, bump.alloc(e)), span)
            }
            / e:exp_infix() { e }
            / e1:simple_exp() "." "(" e2:exp() ")" "<-" e3:exp() {
                let span = (e1.span.start, e3.span.end);
                Expr::new(Set(bump.alloc(e1), bump.alloc(e2), bump.alloc(e3)), span)
            }
            / v:simple_exp()+ {
                let mut v = VecDeque::from_iter(v);
                let e1 = v.pop_front().unwrap();
                if !v.is_empty() {
                    let span = (e1.span.start, v.back().unwrap().span.end);
                    Expr::new(App(bump.alloc(e1), v.into()), span)
                }
                else {
                    e1
                }
            }
        rule exp_infix() -> Expr<'t, 'b> = precedence! {
            x:(@) r:op_rel() y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(BBinOp(r), bump.alloc(x), bump.alloc(y)), span)
            }
            --
            x:(@) "+" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Add), bump.alloc(x), bump.alloc(y)), span)
            }
            x:(@) "-" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Sub), bump.alloc(x), bump.alloc(y)), span)
            }
            x:(@) "+." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FAdd), bump.alloc(x), bump.alloc(y)), span)
            }
            x:(@) "-." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FSub), bump.alloc(x), bump.alloc(y)), span)
            }
            --
            x:(@) "*" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Mul), bump.alloc(x), bump.alloc(y)), span)
            }
            x:(@) "/" y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(IBinOp(Div), bump.alloc(x), bump.alloc(y)), span)
            }
            x:(@) "*." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FMul), bump.alloc(x), bump.alloc(y)), span)
            }
            x:(@) "/." y:@ {
                let span = (x.span.start, y.span.end);
                Expr::new(Binary(FBinOp(FDiv), bump.alloc(x), bump.alloc(y)), span)
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

    use crate::lexer::{Lexer, SelectedLexer};

    use super::*;

    #[test]
    fn test_simple_exp() {
        let bump = &Bump::new();
        assert_eq!(
            test_parser("((*  *)()     )", bump, mincaml::simple_exp).range(),
            7..9
        );
        test_parser("a", bump, mincaml::simple_exp);
        test_parser("a.(0)", bump, mincaml::simple_exp);
        test_parser("a.(0).(0)", bump, mincaml::simple_exp);
    }

    #[test]
    fn test_exp() {
        let bump = &Bump::new();
        let spanned = test_parser("a b c", bump, mincaml::exp);
        assert!(matches!(spanned.node, ExprKind::App(_, _)));
        let spanned = test_parser("Array.make 1 1", bump, mincaml::exp);
        assert!(matches!(spanned.node, ExprKind::ArrayMake(_, _)));
        let spanned = test_parser("not true", bump, mincaml::exp);
        assert!(matches!(spanned.node, ExprKind::Unary(UnOp::Not, _)));
    }

    type PegRule<'a, 'b> = for<'lexer> fn(
        &PegParserImpl<'a, 'lexer>,
        &'b Bump,
        &PegParserImpl<'a, 'lexer>,
    ) -> Result<
        Expr<'a, 'b>,
        peg::error::ParseError<<PegParserImpl<'a, 'lexer> as peg::Parse>::PositionRepr>,
    >;
    fn test_parser<'a, 'b: 'a>(input: &'a str, bump: &'b Bump, f: PegRule<'a, 'b>) -> Expr<'a, 'b> {
        let v = SelectedLexer::new(input).read_to_vec().unwrap();
        // dbg!(&v);
        let p = PegParserImpl::new(&v);
        f(&p, bump, &p).unwrap()
    }
}
