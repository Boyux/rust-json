// ...

use proc_macro::{TokenStream, TokenTree, Delimiter};
use crate::meta::Meta;
use crate::{Error, Result, Parse};
use crate::utils;
use crate::name::Type;

#[derive(Debug)]
pub enum Fields {
    // like "struct Dummy(...)"
    // idents are number
    TupleFields(Vec<TupleFieldItem>),

    // like "struct Dummy { field1: ..., field2: ... }"
    // idents are string
    StructFields(Vec<StructFieldItem>),

    // like "struct Dummy;"
    // without any fields
    NoFields,
}

// shortcuts
impl Fields {
    pub fn index_fields(&self) -> Option<&Vec<TupleFieldItem>> {
        match self {
            Fields::TupleFields(fields) => Some(fields),
            _ => None
        }
    }

    pub fn name_fields(&self) -> Option<&Vec<StructFieldItem>> {
        match self {
            Fields::StructFields(fields) => Some(fields),
            _ => None
        }
    }
}

impl ToString for Fields {
    fn to_string(&self) -> String {
        match self {
            Fields::TupleFields(fields) => {
                let mut fields = fields
                    .iter()
                    .collect::<Vec<_>>();

                // sort fields
                fields.sort_by_key(|&x| x.pos);

                format!("({})", fields
                    .iter()
                    .map(|&x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "))
            }
            Fields::StructFields(fields) => {
                format!("{{\n{}\n}}", fields
                    .iter()
                    .map(|x| x.to_string())
                    .map(|x| format!("    {},", x))
                    .collect::<Vec<_>>()
                    .join("\n"))
            }
            Fields::NoFields => {
                "".to_string()
            }
        }
    }
}

impl Parse for Fields {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing fields."));
        }

        let mut iter = input
            .clone()
            .into_iter()
            .skip_while(|x| {
                match x {
                    TokenTree::Group(group) => {
                        group.delimiter() != Delimiter::Parenthesis && group.delimiter() != Delimiter::Brace
                    }
                    TokenTree::Punct(punct) => punct.as_char() != ';',
                    _ => true
                }
            });

        match &iter.next() {
            // no body found, so we return NoField
            // "struct Dummy;"
            Some(TokenTree::Punct(punct)) if punct.as_char() == ';' => {
                return Ok(Fields::NoFields);
            }

            // body contained by parenthesis
            // "struct Dummy(...)"
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                let mut vec: Vec<TupleFieldItem> = utils::split_tokens_by(group.stream(), ",")
                    .into_iter()
                    .map(|x| TupleFieldItem::parse(x))
                    .collect::<Result<Vec<TupleFieldItem>>>()?;

                // set pos for each field
                let n = vec.iter_mut()
                    .fold(0, |i, x| {
                        x.pos = i;
                        i + 1
                    });

                // guarantee to be valid
                assert_eq!(vec.len(), n);

                // done!
                return Ok(Fields::TupleFields(vec));
            }

            // body contained by brace
            // "struct Dummy { field1: ..., field2: ... }"
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                return Ok(Fields::StructFields(utils::split_tokens_by(group.stream(), ",")
                    .into_iter()
                    .map(|x| StructFieldItem::parse(x))
                    .collect::<Result<Vec<StructFieldItem>>>()?));
            }

            // malformed body
            _ => {
                return Err(Error::new(format!("malformed body of fields: {}.", input)));
            }
        }
    }
}

// IndexFieldItem represents field items in IndexField, including
// meta, vis, position and type for each field.
#[derive(Debug)]
pub struct TupleFieldItem {
    pub meta: Option<Meta>,
    pub vis: Option<Option<String>>,
    pub pos: usize,
    pub ty: Type,
}

impl ToString for TupleFieldItem {
    fn to_string(&self) -> String {
        let mut f = String::new();

        if let Some(meta) = &self.meta {
            f.push_str(&meta.to_string());
            f.push(' ');
        }

        if let Some(vis) = &self.vis {
            f.push_str("pub");
            if let Some(path) = vis {
                f.push_str(&format!("({})", path));
            }
            f.push(' ');
        }

        f.push_str(&self.ty.to_string());

        f
    }
}

impl Parse for TupleFieldItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing indexed field."));
        }

        // make peekable
        let mut peek = input
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

        // parse vis
        let vis = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "pub" => {
                peek.next();
                match peek.peek() {
                    Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                        let path = Some(group.stream().to_string());
                        peek.next();
                        Some(path)
                    }
                    _ => Some(None)
                }
            }
            _ => None
        };

        // parse type
        let ty = Type::parse(peek.collect())?;

        // return with default pos 0
        Ok(TupleFieldItem { meta, vis, ty, pos: 0 })
    }
}


// NameFieldItem represents field items in NameField, including
// meta, vis, name and type for each field.
#[derive(Debug)]
pub struct StructFieldItem {
    pub meta: Option<Meta>,
    pub vis: Option<Option<String>>,
    pub ident: String,
    pub ty: Type,
}

impl ToString for StructFieldItem {
    fn to_string(&self) -> String {
        let mut f = String::new();

        if let Some(meta) = &self.meta {
            f.push_str(&meta.to_string());
            f.push('\n');
        }

        if let Some(vis) = &self.vis {
            f.push_str("pub");
            if let Some(path) = vis {
                f.push_str(&format!("({})", path));
            }
            f.push(' ');
        }

        f.push_str(&format!("{}: {}", &self.ident, &self.ty.to_string()));

        f
    }
}

impl Parse for StructFieldItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing named field."));
        }

        // make peekable
        let mut peek = input
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

        // parse vis
        let vis = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "pub" => {
                peek.next();
                match peek.peek() {
                    Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                        let path = Some(group.stream().to_string());
                        peek.next();
                        Some(path)
                    }
                    _ => Some(None)
                }
            }
            _ => None
        };

        // split ident and type by ':'
        let mut tokens: Vec<TokenStream> = Vec::with_capacity(2);
        let mut token = TokenStream::new();
        let mut hit_colon = false;
        for e in peek.collect::<TokenStream>() {
            if hit_colon {
                token.extend(Some(e));
            } else {
                match e {
                    TokenTree::Punct(ref punct) if punct.as_char() == ':' => {
                        hit_colon = true;
                        tokens.push(token.clone());
                        token = TokenStream::new();
                    }
                    other => {
                        token.extend(Some(other))
                    }
                }
            }
        }
        tokens.push(token);

        // validate
        if tokens.len() != 2 {
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
                Some(TokenTree::Ident(ident)) => ident.to_string(),
                Some(other) => return Err(Error::new(format!("field name should be an ident, got {}.", other))),
                _ => return Err(Error::new(format!("malformed field: {}.", input)))
            }
            _ => return Err(Error::new(format!("malformed field: {}.", input)))
        };

        // return
        Ok(StructFieldItem { meta, vis, ident, ty })
    }
}
