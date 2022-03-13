extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{self, punctuated::Punctuated, token::Comma, DataEnum, Variant};

#[derive(Debug)]
struct Value {
    variant: syn::Ident,
    symbol: String,
}

#[proc_macro_derive(KaonToken, attributes(symbol))]
pub fn token_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    token_impl(&ast)
}

fn token_impl(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    let match_block = match &ast.data {
        syn::Data::Enum(DataEnum { variants, .. }) => parse_variants(variants).unwrap(),
        _ => panic!(),
    };

    let mut extended = quote! {};

    for item in match_block {
        let sym = item.symbol.clone();
        let v = item.variant;
        extended.extend(quote! {
            #sym => Self::#v,
        })
    }

    let gen = quote! {
        impl #name {
            pub fn symbol(sym: &str) -> Self {
                match sym {
                    #extended
                    _ => unimplemented!(),
                }
            }
        }
    };

    gen.into()
}

fn parse_variants(variants: &Punctuated<Variant, Comma>) -> Result<Vec<Value>, String> {
    let variants: Vec<_> = variants
        .iter()
        .map(|v| {
            if v.attrs.is_empty() {
                panic!("no attribute `token` found for variant");
            }

            let symbol = match v.attrs[0].parse_meta().unwrap() {
                syn::Meta::List(meta) => {
                    let nested = meta.nested;

                    match nested[0].clone() {
                        syn::NestedMeta::Lit(syn::Lit::Str(lit)) => lit.value(),
                        _ => panic!("I was waiting right here. And it 'just' broke."),
                    }
                }
                _ => panic!("And he yeets the computer"),
            };

            Value {
                variant: v.ident.clone(),
                symbol: symbol.to_string(),
            }
        })
        .collect();

    Ok(variants)
}
