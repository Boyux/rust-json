// ...

use proc_macro::{TokenStream, TokenTree, Delimiter};
use crate::{Error, Result, Parse};
use crate::name::Type;
use crate::utils;

// Params represents function's parameters
#[derive(Debug)]
pub struct Params {
    pub items: Vec<ParamItem>,
}

impl ToString for Params {
    fn to_string(&self) -> String {
        format!("({})",
                self.items
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "))
    }
}

impl Parse for Params {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing param item."));
        }

        // expect one token for param: (...)
        let token = input
            .clone()
            .into_iter()
            .next();

        match &token {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                return Ok(Params {
                    items: utils::split_tokens_by(group.stream(), ",")
                        .into_iter()
                        .map(|x| ParamItem::parse(x))
                        .collect::<Result<Vec<ParamItem>>>()?
                });
            }
            _ => {
                return Err(Error::new(format!("malformed param values: {}.", input)));
            }
        }
    }
}

#[derive(Debug)]
pub struct ParamItem {
    // an ident is optional because when it is self,
    // the ident can be omitted.
    pub ident: Option<String>,
    pub ty: Type,
}

impl ToString for ParamItem {
    fn to_string(&self) -> String {
        let mut s = String::new();

        if let Some(ident) = &self.ident {
            s.push_str(&ident);
            s.push_str(": ");
        }

        s.push_str(&self.ty.to_string());

        s
    }
}

impl Parse for ParamItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing param item."));
        }

        // split ident and type by ':'
        let mut tokens = utils::split_tokens_by(input.clone(), ":");

        // validate
        if ![1, 2].contains(&tokens.len()) {
            return Err(Error::new(format!("malformed field: {}.", input)));
        }

        // parse type
        let ty = match tokens.pop() {
            Some(token) => Type::parse(token)?,
            _ => return Err(Error::new(format!("malformed field: {}.", input)))
        };

        // parse name
        let ident = match tokens.pop() {
            Some(token) => match token.into_iter().next() {
                Some(TokenTree::Ident(ident)) => Some(ident.to_string()),
                Some(other) => return Err(Error::new(format!("field name should be an ident, got {}.", other))),
                _ => None,
            }
            _ => None,
        };

        // return
        Ok(ParamItem { ident, ty })
    }
}