#![feature(extend_one)]

use proc_macro::TokenStream;

mod doc;

#[proc_macro_attribute]
pub fn counterpart(attr: TokenStream, item: TokenStream) -> TokenStream {
    doc::counterpart(attr, item)
}
