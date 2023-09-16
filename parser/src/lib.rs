use syntax::Expr;

mod lexer;
#[cfg(feature = "plex")]
mod plex;
#[cfg(feature = "plex")]
type SelectedLexer<'a> = plex::PlexLexer<'a>;

mod parser;
#[cfg(feature = "peg")]
mod peg;
#[cfg(feature = "peg")]
type SelectedParser = peg::PegParser;
#[cfg(feature = "lalrpop")]
mod lalrpop;
#[cfg(feature = "lalrpop")]
type SelectedParser = lalrpop::LalrpopParser;

pub type Error<'a> = parser::Error<'a>;

pub fn lex_and_parse(input: &str) -> Result<Expr<'_>, Error<'_>> {
    use lexer::Lexer;
    use parser::Parser;

    SelectedParser::parse(SelectedLexer::new(input))
}
