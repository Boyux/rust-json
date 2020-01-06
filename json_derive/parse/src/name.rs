// ...

use proc_macro::{TokenStream, TokenTree, Delimiter};
use std::iter::FromIterator;
use std::ops::{Sub, Add};
use crate::{Error, Result, Parse};
use crate::utils;

// Name represents a identifier of:
//     1. struct name => struct #name;
//     2. fn name => fn #name(...) -> ...;
//     3. trait name => trait #name ...;
//     4. primitive type with out reference/lifetime/mut/dyn => Vec<T>
//     5. some other ident...
// including it's generics parameters. Generally speaking, generics
// parameters should be bound with ident so we make this type
// instead of using a simple String.
#[derive(Debug, PartialEq, Eq)]
pub struct Name {
    // prefix is like std::iter::...
    pub prefixes: Vec<String>,
    pub ident: String,
    pub generics: Vec<Type>,
}

// token_num means how many token it takes. In Name case,
// a simple ident takes 1 token, a lifetime takes 2, and
// generics takes more than 3, so we have a nested count.
impl Name {
    pub fn __token_num(&self) -> usize {
        // means no generic parameters
        if self.is_lifetime() {
            return 2;
        }

        let n = self.prefixes.len() * 3;

        // Fn/FnMut/FnOnce/fn case
        if self.ident.starts_with("Fn (")
            || self.ident.starts_with("FnMut (")
            || self.ident.starts_with("FnOnce (")
            || self.ident.starts_with("fn (") {
            return n + self.ident
                .parse::<TokenStream>()
                .unwrap()
                .into_iter()
                .count();
        }

        // a simple ident token
        if self.generics.is_empty() {
            return n + 1;
        }

        // each generic parameter contains its own token_num and
        // an extra comma, so we should add the comma count for
        // each generic. And because the last generic doesn't have
        // a comma, so we subtract one. Finally we add the ident
        // token and '<' '>' token, so it should be look like:
        n + self.generics
            .iter()
            .map(|x| x.__token_num())
            .map(|x| x + 1)
            .sum::<usize>()
            .sub(1)
            .add(3)
    }

    // provide a shortcut to judge whether this name value
    // is a lifetime, which should starts with a '\'' punct.
    pub fn is_lifetime(&self) -> bool {
        self.ident.starts_with('\'')
    }
}

impl ToString for Name {
    fn to_string(&self) -> String {
        let ident = if self.generics.is_empty() {
            self.ident.clone()
        } else {
            format!(
                "{}<{}>",
                self.ident,
                self.generics
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        };
        if !self.prefixes.is_empty() {
            let prefix = self.prefixes.join("::");
            format!("{}::{}", prefix, ident)
        } else {
            ident
        }
    }
}

impl Parse for Name {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing name."));
        }

        // split tokens
        let splits = utils::split_tokens_by(input.clone(), "::");
        if splits.is_empty() {
            return Err(Error::new(format!("malformed name: {}.", input)));
        }

        // handle prefixes
        let prefixes = splits.iter()
            .take(splits.len() - 1)
            .map(|x| x.to_string())
            .collect::<Vec<_>>();

        let stream = splits
            .into_iter()
            .last()
            .unwrap()
            .into_iter()
            .collect::<Vec<_>>();

        // HINT: specially case for Fn/FnMut/FnOnce
        if stream.len() >= 2 {
            match (&stream[0], &stream[1]) {
                (TokenTree::Ident(f), TokenTree::Group(g)) => {
                    if ["Fn", "FnMut", "FnOnce", "fn"].contains(&f.to_string().as_str())
                        && g.delimiter() == Delimiter::Parenthesis {
                        return Ok(Name {
                            prefixes,
                            ident: TokenStream::from_iter(stream).to_string(),
                            generics: Vec::new(),
                        });
                    }
                }
                _ => ()
            }
        }

        match stream.len() {
            1 => {
                match &stream[0] {
                    TokenTree::Ident(ident) => Ok(Name { prefixes, ident: ident.to_string(), generics: Vec::new() }),

                    // tuple
                    TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                        Ok(Name { prefixes, ident: group.to_string(), generics: Vec::new() })
                    }

                    // array & slice
                    TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket => {
                        Ok(Name { prefixes, ident: group.to_string(), generics: Vec::new() })
                    }

                    _ => Err(Error::new(format!("single identifier should be Ident, got {}.", input)))
                }
            }

            2 => {
                match &stream[0] {
                    TokenTree::Punct(punct) if punct.as_char() == '\'' => {
                        match &stream[1] {
                            TokenTree::Ident(ident) => Ok(Name {
                                prefixes,
                                ident: format!("'{}", ident.to_string()),
                                generics: Vec::new(),
                            }),
                            other => {
                                Err(Error::new(format!("lifetime identifier should be Ident, got {}.", other)))
                            }
                        }
                    }
                    other => {
                        Err(Error::new(format!("lifetime identifier should start with \', got {}.", other)))
                    }
                }
            }

            n if n > 3 => {
                match (&stream[0], &stream[1], &stream[stream.len() - 1]) {
                    (TokenTree::Ident(ident),
                        TokenTree::Punct(left),
                        TokenTree::Punct(right)) if left.as_char() == '<'
                        && right.as_char() == '>' => {
                        Ok(Name {
                            prefixes,
                            ident: ident.to_string(),
                            generics: utils::split_tokens_by(
                                TokenStream::from_iter(stream[2..stream.len() - 1].to_vec()), ",")
                                .into_iter()
                                .map(|x| Type::parse(x))
                                .collect::<Result<Vec<Type>>>()?,
                        })
                    }
                    _ => Err(Error::new(format!("malformed identifier with generics: {}.", input)))
                }
            }

            _ => Err(Error::new(format!("malformed identifier: {}.", input)))
        }
    }
}

// Type represents a mixed type, looks like: (&('a) (mut)) (dyn) (impl) Vec<T>
#[derive(Debug, PartialEq, Eq)]
pub struct Type {
    // like Iterator<Item=i32>
    //               ^^^^
    pub ident: Option<String>,
    // use a vector instead of a single Name is for
    // handling multiple trait when is_imp is true
    pub names: Vec<Name>,

    // reference options, only if is_ref is true then
    // lifetime/is_mut is valid
    pub is_ref: bool,
    pub lifetime: Option<Name>,
    pub is_mut: bool,

    // is dyn type => Box<dyn Read>
    pub is_dyn: bool,

    // is impl type => impl Read + Write
    pub is_imp: bool,
}

impl Type {
    pub fn __token_num(&self) -> usize {
        let mut n = self.names
            .iter()
            .map(|x| x.__token_num())
            // add '+' if is_imp is true
            .map(|x| x + 1)
            .sum::<usize>()
            .sub(1);

        if self.ident.is_some() {
            n += 2;
        }

        if self.is_ref {
            n += 1;
        }

        if self.lifetime.is_some() {
            n += 2;
        }

        if self.is_mut {
            n += 1;
        }

        if self.is_dyn {
            n += 1;
        }

        if self.is_imp {
            n += 1;
        }

        n
    }

    pub fn is_lifetime(&self) -> bool {
        self.names.len() == 1 && self.names[0].is_lifetime()
    }

    // to produce a type that can use static method like T::method
    pub fn used_ty(&self) -> String {
        let mut ty = String::new();
        if self.is_ref {
            ty.push('&');
        }
        if self.is_dyn {
            ty.push_str("dyn ");
        }
        ty.push_str(&self.names
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(" + "));
        ty
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        let mut ty = String::new();

        if self.is_ref {
            ty.push('&');

            if let Some(lifetime) = &self.lifetime {
                ty.push_str(&lifetime.to_string());
            }

            if self.is_mut {
                ty.push_str(" mut ");
            }
        }

        if self.is_dyn {
            ty.push_str("dyn ");
        }

        if self.is_imp {
            ty.push_str("impl ");
        }

        if let Some(ident) = &self.ident {
            ty.push_str(ident);
            ty.push_str(" = ");
        }

        ty.push_str(&self.names
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(" + "));

        ty
    }
}

impl Parse for Type {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing type."));
        }

        // split by '='
        let mut tokens = utils::split_tokens_by(input.clone(), "=");

        // handle ident
        let ident = if tokens.len() > 1 {
            Some(tokens[0].to_string())
        } else {
            None
        };

        // malformed type
        if tokens.len() != 1 && tokens.len() != 2 {
            return Err(Error::new(format!("malformed type: {}.", tokens.into_iter().collect::<TokenStream>())));
        }

        // make a peekable for inspect reference/lifetime and mutable
        let mut peek = tokens
            .pop()
            .unwrap()
            .into_iter()
            .peekable();

        // parse if_ref
        let is_ref = match peek.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '&' => {
                peek.next();
                true
            }
            _ => false
        };

        // parse lifetime
        let lifetime = if is_ref {
            match peek.peek() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == '\'' => {
                    let input = peek
                        .clone()
                        .take(2)
                        .collect::<TokenStream>();

                    // consume peek
                    peek.nth(1);

                    Some(Name::parse(input)?)
                }
                _ => None
            }
        } else {
            None
        };

        // parse is_mut
        let is_mut = if is_ref {
            match peek.peek() {
                Some(TokenTree::Ident(ident)) if &ident.to_string() == "mut" => {
                    peek.next();
                    true
                }
                _ => false
            }
        } else {
            false
        };

        // parse is_dyn
        let is_dyn = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "dyn" => {
                peek.next();
                true
            }
            _ => false
        };

        // parse is_imp
        let is_imp = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "impl" => {
                peek.next();
                true
            }
            _ => false
        };

        // validate
        if peek.peek().is_none()
            || (is_ref && (is_dyn || is_imp))
            || (is_dyn && (is_ref || is_imp))
            || (is_imp && (is_ref || is_dyn)) {
            return Err(Error::new(format!("malformed type: {}.", input)));
        }

        // handle primitive type name
        let names = utils::split_tokens_by(peek.collect::<TokenStream>(), "+")
            .into_iter()
            .map(|x| Name::parse(x))
            .collect::<Result<Vec<Name>>>()?;

        // return
        Ok(Type { ident, names, is_ref, lifetime, is_mut, is_dyn, is_imp })
    }
}