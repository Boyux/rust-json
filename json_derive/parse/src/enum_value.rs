// ...

use proc_macro::{TokenStream, TokenTree, Delimiter};
use crate::meta::Meta;
use crate::field::Fields;
use crate::{Error, Result, Parse};
use crate::utils;

#[derive(Debug)]
pub struct EnumValues {
    pub items: Vec<EnumValueItem>,
}

impl ToString for EnumValues {
    fn to_string(&self) -> String {
        format!("{{\n{}\n}}",
                self.items
                    .iter()
                    .map(|x| x.to_string())
                    .map(|x| format!("    {},", x))
                    .collect::<Vec<_>>()
                    .join("\n"))
    }
}

impl Parse for EnumValues {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing enum values."));
        }

        // expect one token for enum: {...}
        let token = input
            .clone()
            .into_iter()
            .next();

        match &token {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                return Ok(EnumValues {
                    items: utils::split_tokens_by(group.stream(), ",")
                        .into_iter()
                        .map(|x| EnumValueItem::parse(x))
                        .collect::<Result<Vec<EnumValueItem>>>()?
                });
            }
            _ => {
                return Err(Error::new(format!("malformed enum values: {}.", input)));
            }
        }
    }
}

#[derive(Debug)]
pub struct EnumValueItem {
    // #[...]
    pub meta: Option<Meta>,

    // ident
    pub ident: String,

    // none
    // tuple(...)
    // struct{x: ..., y: ...}
    pub fields: Fields,

    // discrimination: ... = 1
    pub disc: Option<isize>,
}

impl ToString for EnumValueItem {
    fn to_string(&self) -> String {
        let mut s = String::new();

        if let Some(meta) = &self.meta {
            s.push_str(&meta.to_string());
            s.push('\n');
        }

        s.push_str(&self.ident);
        s.push_str(&self.fields.to_string());

        if let Some(disc) = self.disc {
            s.push_str(" = ");
            s.push_str(&disc.to_string());
        }

        s
    }
}

impl Parse for EnumValueItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing enum values."));
        }

        // first we parse discrimination
        let tokens = utils::split_tokens_by(input.clone(), "=");
        let disc = if tokens.len() >= 2 {
            let s = tokens[1].to_string();
            match s.parse::<isize>() {
                Ok(n) => Some(n),
                _ => {
                    return Err(Error::new(format!("enum discrimination expects isize, got {}.", s)));
                }
            }
        } else {
            None
        };

        if tokens.is_empty() {
            return Err(Error::new(format!("malformed enum value: {}.", input)));
        }

        // make peekable
        let mut peek = tokens[0]
            .clone()
            .into_iter()
            .peekable();

        // parse meta
        let meta = match peek.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '#' => {
                let meta = Meta::parse(peek.clone().collect())?;
                // skip meta tokens
                peek.nth(meta.__token_num() - 1);
                Some(meta)
            }
            _ => None
        };

        // parse ident
        let ident = match peek.peek() {
            Some(TokenTree::Ident(ident)) => {
                let ident = ident.to_string();
                // skip ident
                peek.nth(0);
                ident
            }
            Some(other) => {
                return Err(Error::new(format!("enum value expects ident, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed enum value: {}.", input)));
            }
        };

        // parse fields
        let fields = match peek.peek() {
            Some(_) => Fields::parse(peek.collect())?,
            None => Fields::NoFields
        };

        // return
        Ok(EnumValueItem { meta, ident, fields, disc })
    }
}