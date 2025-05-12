use quote::quote;
use quote::ToTokens;
use syn::{parse_macro_input, Expr, Item, Lit, Meta, Path};

use crate::util::is_doc_comment;

pub fn counterpart(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut counterpart_path = parse_macro_input!(attr as Path)
        .to_token_stream()
        .to_string();
    counterpart_path.retain(|c| !c.is_whitespace());

    let mut attributed_item = parse_macro_input!(item as Item);
    let attr_mut = match &mut attributed_item {
        Item::Const(item) => Some(&mut item.attrs),
        Item::Enum(item) => Some(&mut item.attrs),
        Item::Fn(item) => Some(&mut item.attrs),
        Item::ForeignMod(item) => Some(&mut item.attrs),
        Item::Macro(item) => Some(&mut item.attrs),
        Item::Mod(item) => Some(&mut item.attrs),
        Item::Static(item) => Some(&mut item.attrs),
        Item::Struct(item) => Some(&mut item.attrs),
        Item::Trait(item) => Some(&mut item.attrs),
        Item::TraitAlias(item) => Some(&mut item.attrs),
        Item::Type(item) => Some(&mut item.attrs),
        Item::Use(item) => Some(&mut item.attrs),
        _ => None,
    };
    if let Some(attrs) = attr_mut {
        let last_comment = attrs.iter().rev().find_map(|attr| {
            if !is_doc_comment(attr) {
                return None;
            };
            let Meta::NameValue(attr) = &attr.meta else {
                return None;
            };
            let Expr::Lit(expr) = &attr.value else {
                return None;
            };
            let Lit::Str(lit) = &expr.lit else {
                return None;
            };
            Some(lit)
        });
        let contain_nonempty = last_comment.is_some_and(|lit| !lit.value().is_empty());
        if contain_nonempty {
            attrs.push(syn::parse_quote!(#[doc = ""]));
            let doc = format!("See also the `rustc` counterpart: `{counterpart_path}`.");
            attrs.push(syn::parse_quote!(#[doc = #doc]));
        } else {
            let doc = format!("`rustc` counterpart: `{counterpart_path}`");
            attrs.push(syn::parse_quote!(#[doc = #doc]));
        }
    };

    proc_macro::TokenStream::from(quote! {
        #attributed_item
    })
}
