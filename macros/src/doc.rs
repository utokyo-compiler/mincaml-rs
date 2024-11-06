use proc_macro::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::{parse_macro_input, Path};

pub fn counterpart(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(attr as Path)
        .to_token_stream()
        .to_string();
    input.retain(|c| !c.is_whitespace());

    let doc = format!("rustc counterpart: `{input}`");
    let mut stream = TokenStream::from(quote! {
        #[doc = #doc]
    });
    stream.extend_one(item);
    stream
}
