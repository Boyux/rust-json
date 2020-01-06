// ...

extern crate proc_macro;

pub mod utils;
pub mod meta;
pub mod name;
pub mod field;
pub mod generic;
pub mod enum_value;
pub mod param;

use std::result;
use std::error;
use std::convert::Into;
use proc_macro::{TokenStream, TokenTree, Delimiter, Group, Literal};

use crate::meta::Meta;
use crate::generic::{Generics, WhereClause};
use crate::field::Fields;
use crate::enum_value::EnumValues;
use crate::param::Params;
use crate::name::Type;

// Parse Error, contains an &'static str
#[derive(Debug)]
pub struct Error {
    inner: Box<dyn error::Error>
}

// Construct Error
impl Error {
    pub fn new<E: Into<Box<dyn error::Error>>>(error: E) -> Error {
        Error { inner: error.into() }
    }
}

// Parse Result
pub type Result<T> = result::Result<T, Error>;

// Parse trait
pub trait Parse: Sized {
    fn parse(input: TokenStream) -> Result<Self>;
}

// a general function to parse Item
pub fn parse<T: Parse>(input: TokenStream) -> Result<T> {
    T::parse(input)
}

// Item represents rust item
#[derive(Debug)]
pub enum Item {
    // common items
    Struct(StructItem),
    Enum(EnumItem),
    Fn(FnItem),

    // other items omitted here...

    // Impl(ImplItem)
    // Trait(TraitItem)
}

// shortcut methods
impl Item {
    pub fn struct_item(&self) -> Option<&StructItem> {
        match self {
            Item::Struct(struct_item) => Some(struct_item),
            _ => None
        }
    }

    pub fn enum_item(&self) -> Option<&EnumItem> {
        match self {
            Item::Enum(enum_item) => Some(enum_item),
            _ => None
        }
    }

    pub fn fn_item(&self) -> Option<&FnItem> {
        match self {
            Item::Fn(fn_item) => Some(fn_item),
            _ => None
        }
    }
}

impl ToString for Item {
    fn to_string(&self) -> String {
        match self {
            Item::Struct(struct_item) => struct_item.to_string(),
            Item::Enum(enum_item) => enum_item.to_string(),
            Item::Fn(fn_item) => fn_item.to_string(),
        }
    }
}

impl Parse for Item {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing item."));
        }

        let keyword = input
            .clone()
            .into_iter()
            // skip meta tokens and vis token
            .skip_while(|x| {
                match x {
                    TokenTree::Punct(punct) => punct.as_char() == '#',
                    TokenTree::Group(group) => {
                        group.delimiter() == Delimiter::Bracket || group.delimiter() == Delimiter::Parenthesis
                    }
                    TokenTree::Ident(ident) => {
                        ["async", "const", "unsafe", "extern", "pub"].contains(&ident.to_string().as_str())
                    }
                    TokenTree::Literal(_) => true,
                }
            })
            .next();

        match &keyword {
            Some(kw) if &kw.to_string() == "struct" => {
                return Ok(Item::Struct(StructItem::parse(input)?));
            }
            Some(kw) if &kw.to_string() == "enum" => {
                return Ok(Item::Enum(EnumItem::parse(input)?));
            }
            Some(kw) if &kw.to_string() == "fn" => {
                return Ok(Item::Fn(FnItem::parse(input)?));
            }
            Some(other) => {
                return Err(Error::new(format!("expect keyword struct/enum/fn ... etc, got {}.", other)));
            }
            None => {
                return Err(Error::new(format!("malformed item: {}.", input)));
            }
        }
    }
}

// Struct represents a struct item, including meta,
// vis, ident, generics, fields. Additionally, meta
// and generics are optional.
#[derive(Debug)]
pub struct StructItem {
    // meta above struct => #[...]
    pub meta: Option<Meta>,

    // visibility of struct => pub ...
    pub vis: Option<Option<String>>,

    // struct name
    pub ident: String,

    // generics => <...>
    pub generics: Option<Generics>,

    // where clauses => where ...
    // well, where is a keyword in rust, so we
    // have to give the field a long name.
    pub where_clause: Option<WhereClause>,

    // fields
    pub fields: Fields,
}

impl ToString for StructItem {
    fn to_string(&self) -> String {
        let mut s = String::new();

        if let Some(meta) = &self.meta {
            s.push_str(&meta.to_string());
            s.push('\n');
        }

        if let Some(vis) = &self.vis {
            s.push_str("pub");
            if let Some(path) = vis {
                s.push_str(&format!("({})", path));
            }
            s.push(' ');
        }

        s.push_str("struct ");
        s.push_str(&self.ident);

        if let Some(generics) = &self.generics {
            s.push_str(&generics.to_string());
        }

        s.push(' ');

        match &self.fields {
            Fields::NoFields => {
                s.push(';');
            }
            fields @ Fields::TupleFields(_) => {
                s.push_str(&fields.to_string());
                if let Some(where_clause) = &self.where_clause {
                    s.push_str(&where_clause.to_string());
                }
                s.push(';');
            }
            fields @ Fields::StructFields(_) => {
                if let Some(where_clause) = &self.where_clause {
                    s.push_str(&where_clause.to_string());
                }
                s.push_str(&fields.to_string());
            }
        }

        s
    }
}

impl Parse for StructItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing struct."));
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

        // parse struct keyword
        match &peek.next() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "struct" => (),
            Some(other) => {
                return Err(Error::new(format!("expect struct keyword, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed struct: {}.", input)));
            }
        }

        // parse struct ident
        let ident = match &peek.next() {
            Some(TokenTree::Ident(ident)) => ident.to_string(),
            Some(other) => {
                return Err(Error::new(format!("expect Ident for struct name, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed struct: {}.", input)));
            }
        };

        // parse generics
        let generics = match peek.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                let generics = Generics::parse(peek.clone().collect())?;
                // skip generics tokens
                peek.nth(generics.__token_num() - 1);
                Some(generics)
            }
            _ => None
        };

        // parse fields
        let (where_clause, fields) = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "where" => {
                let where_clause = WhereClause::parse(peek.clone().collect())?;
                // skip where clauses
                peek.nth(where_clause.__token_num() - 1);
                (Some(where_clause), Fields::parse(peek.collect())?)
            }
            Some(TokenTree::Punct(punct)) if punct.as_char() == ';' => {
                (None, Fields::NoFields)
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                (None, Fields::parse(peek.collect())?)
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                let fields = Fields::parse(TokenTree::from(group.clone()).into())?;
                // skip fields token and parse where clauses
                peek.nth(0);
                match peek.peek() {
                    Some(TokenTree::Ident(ident)) if &ident.to_string() == "where" => {
                        (Some(WhereClause::parse(peek.clone().collect())?), fields)
                    }
                    Some(TokenTree::Punct(punct)) if punct.as_char() == ';' => {
                        (None, fields)
                    }
                    _ => {
                        return Err(Error::new(format!("malformed struct: {}.", input)));
                    }
                }
            }
            _ => {
                return Err(Error::new(format!("malformed struct: {}.", input)));
            }
        };

        // return
        Ok(StructItem { meta, vis, ident, generics, where_clause, fields })
    }
}

// enum, same as struct except it contains values instead of fields
#[derive(Debug)]
pub struct EnumItem {
    // #[...]
    pub meta: Option<Meta>,

    // pub
    pub vis: Option<Option<String>>,

    // ident
    pub ident: String,

    // <'a, T>
    pub generics: Option<Generics>,

    // where: ...
    pub where_clause: Option<WhereClause>,

    // values
    pub values: EnumValues,
}

impl ToString for EnumItem {
    fn to_string(&self) -> String {
        let mut s = String::new();

        if let Some(meta) = &self.meta {
            s.push_str(&meta.to_string());
            s.push('\n');
        }

        if let Some(vis) = &self.vis {
            s.push_str("pub");
            if let Some(path) = vis {
                s.push_str(&format!("({})", path));
            }
            s.push(' ');
        }

        s.push_str("enum ");
        s.push_str(&self.ident);

        if let Some(generics) = &self.generics {
            s.push_str(&generics.to_string());
        }

        s.push(' ');

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&where_clause.to_string());
        }

        s.push(' ');
        s.push_str(&self.values.to_string());

        s
    }
}

impl Parse for EnumItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing enum."));
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

        // parse enum keyword
        match &peek.next() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "enum" => (),
            Some(other) => {
                return Err(Error::new(format!("expect enum keyword, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed enum: {}.", input)));
            }
        }

        // parse enum ident
        let ident = match &peek.next() {
            Some(TokenTree::Ident(ident)) => ident.to_string(),
            Some(other) => {
                return Err(Error::new(format!("expect Ident for enum name, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed enum: {}.", input)));
            }
        };

        // parse generics
        let generics = match peek.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                let generics = Generics::parse(peek.clone().collect())?;
                // skip generics tokens
                peek.nth(generics.__token_num() - 1);
                Some(generics)
            }
            _ => None
        };

        // parse enum values
        let (where_clause, values) = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "where" => {
                let where_clause = WhereClause::parse(peek.clone().collect())?;
                // skip where clauses
                peek.nth(where_clause.__token_num() - 1);
                (Some(where_clause), EnumValues::parse(peek.collect())?)
            }
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                (None, EnumValues::parse(peek.collect())?)
            }
            _ => {
                return Err(Error::new(format!("malformed enum: {}.", input)));
            }
        };

        // return
        Ok(EnumItem { meta, vis, ident, generics, where_clause, values })
    }
}

/////////////
// fn here //
/////////////

#[derive(Debug)]
pub struct FnItem {
    pub meta: Option<Meta>,
    pub vis: Option<Option<String>>,

    // async
    pub asyncness: bool,

    // const
    pub constness: bool,

    // unsafe
    pub unsafety: bool,

    // extern ABI
    pub extern_abi: Option<Literal>,

    pub ident: String,
    pub generics: Option<Generics>,
    pub params: Params,
    pub return_ty: Option<Type>,
    pub where_clause: Option<WhereClause>,
    pub body: Group,
}

impl ToString for FnItem {
    fn to_string(&self) -> String {
        let mut s = String::new();

        if let Some(meta) = &self.meta {
            s.push_str(&meta.to_string());
            s.push('\n');
        }

        if let Some(vis) = &self.vis {
            s.push_str("pub");
            if let Some(path) = vis {
                s.push_str(&format!("({})", path));
            }
            s.push(' ');
        }

        if self.asyncness {
            s.push_str("async ");
        }

        if self.constness {
            s.push_str("const ");
        }

        if self.unsafety {
            s.push_str("unsafe ");
        }

        if let Some(abi) = &self.extern_abi {
            s.push_str(&format!("extern {} ", abi.to_string()));
        }

        s.push_str("fn ");
        s.push_str(&self.ident);

        if let Some(generics) = &self.generics {
            s.push_str(&generics.to_string());
        }

        s.push_str(&self.params.to_string());
        s.push(' ');

        if let Some(return_ty) = &self.return_ty {
            s.push_str(&format!("-> {}", return_ty.to_string()));
        }

        s.push(' ');

        if let Some(where_clause) = &self.where_clause {
            s.push_str(&where_clause.to_string());
        }

        s.push(' ');
        s.push_str(&self.body.to_string());

        s
    }
}

impl Parse for FnItem {
    fn parse(input: TokenStream) -> Result<Self> {
        // handle empty input
        if input.is_empty() {
            return Err(Error::new("got empty input when parsing fn."));
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

        // parse async
        let asyncness = match &peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "async" => {
                peek.next();
                true
            }
            _ => false
        };

        // parse const
        let constness = match &peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "const" => {
                peek.next();
                true
            }
            _ => false
        };

        // parse unsafe
        let unsafety = match &peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "unsafe" => {
                peek.next();
                true
            }
            _ => false
        };

        // parse extern abi
        let extern_abi = match &peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "extern" => {
                peek.next();
                match peek.peek() {
                    Some(TokenTree::Literal(lit)) => {
                        let extern_abi = Some(lit.clone());
                        peek.next();
                        extern_abi
                    }
                    _ => None
                }
            }
            _ => None
        };

        // parse fn keyword
        match &peek.next() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "fn" => (),
            Some(other) => {
                return Err(Error::new(format!("expect fn keyword, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed fn: {}.", input)));
            }
        };

        // parse fn ident
        let ident = match &peek.next() {
            Some(TokenTree::Ident(ident)) => ident.to_string(),
            Some(other) => {
                return Err(Error::new(format!("expect Ident for fn name, got {}.", other)));
            }
            _ => {
                return Err(Error::new(format!("malformed fn: {}.", input)));
            }
        };

        // parse generics
        let generics = match peek.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                let generics = Generics::parse(peek.clone().collect())?;
                // skip generics tokens
                peek.nth(generics.__token_num() - 1);
                Some(generics)
            }
            _ => None
        };

        // parse params
        let params = match peek.peek() {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                let params = Params::parse(peek.clone().take(1).collect())?;
                peek.next();
                params
            }
            _ => {
                return Err(Error::new(format!("malformed fn: {}.", input)));
            }
        };

        // parse return type
        let return_ty = match peek.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '-' => {
                peek.next();
                match peek.peek() {
                    Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => {
                        peek.next();
                        let return_ty = Type::parse(peek.clone()
                            .take_while(|x| {
                                match x {
                                    TokenTree::Group(group) => group.delimiter() != Delimiter::Brace,
                                    TokenTree::Ident(ident) => &ident.to_string() != "where",
                                    _ => true
                                }
                            })
                            .collect())?;
                        peek.nth(return_ty.__token_num() - 1);
                        Some(return_ty)
                    }
                    _ => {
                        return Err(Error::new(format!("malformed fn: {}.", input)));
                    }
                }
            }
            _ => None
        };

        // parse where clause
        let where_clause = match peek.peek() {
            Some(TokenTree::Ident(ident)) if &ident.to_string() == "where" => {
                let where_clause = WhereClause::parse(peek.clone().collect())?;
                // skip where clauses
                peek.nth(where_clause.__token_num() - 1);
                Some(where_clause)
            }
            _ => None
        };

        // parse fn body
        let body = match &peek.next() {
            Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace => {
                group.clone()
            }
            _ => {
                return Err(Error::new(format!("malformed fn: {}.", input)));
            }
        };

        // return
        Ok(FnItem {
            meta,
            asyncness,
            constness,
            unsafety,
            extern_abi,
            vis,
            ident,
            generics,
            params,
            return_ty,
            where_clause,
            body,
        })
    }
}