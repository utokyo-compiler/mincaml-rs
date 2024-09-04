use context::ParsingContext;
use syntax::Expr;

pub mod context;
mod lexer;
mod parser;

pub type Error<'input> = parser::Error<'input>;

pub fn lex_and_parse<'input, 'ctx>(
    ctx: &'ctx ParsingContext<'ctx>,
    input: &'input str,
) -> Result<Expr<'ctx>, Error<'input>> {
    use lexer::Lexer;
    use parser::Parser;

    parser::SelectedParser::parse(
        parser::Allocator::new(ctx),
        lexer::SelectedLexer::new(input),
    )
}
