extern crate proc_macro;

use proc_macro::TokenStream;
use json_quote::quote;
use json_parse::Item;
use json_parse::field::Fields;

#[proc_macro_derive(JsonEncode, attributes(key, ignore))]
pub fn json_encode(input: TokenStream) -> TokenStream {
    let item: Item = json_parse::parse(input).expect("error parsing item");
    match item {
        Item::Struct(s) => {
            match s.fields {
                Fields::StructFields(v) => {
                    let v: Vec<(String, String)> = v.iter()
                        .filter(|x| {
                            if let Some(meta) = &x.meta {
                                return !meta.idents().into_iter().any(|x| x == "ignore");
                            }
                            true
                        })
                        .map(|x| {
                            let k = if let Some(meta) = &x.meta {
                                if let Some(key) = meta.get("key") {
                                    key.string().expect("key attribute should be string").to_string()
                                } else {
                                    x.ident.clone()
                                }
                            } else {
                                x.ident.clone()
                            };
                            (format!("\"{}\".to_string()", k), x.ident.clone())
                        })
                        .collect();
                    quote! {
                        impl#(#(&s.generics))? json::ToJson for #(s.ident)#(#(&s.generics):x #(x.only_idents()))?
                        #(#(&s.where_clause))?
                        {
                            fn to_json(&self) -> json::Json {
                                let map = std::collections::HashMap::with_capacity(#(v.len()));
                                let mut js = json::Json::Object(map);
                                #(#v:x
                                    match js.add_option(#(x.0), json::ToJson::to_json(&self.#(x.1)), true) {
                                        Ok(_) => (),
                                        Err(_) => unreachable!()
                                    };
                                )*
                                js
                            }
                        }
                    }
                }
                _ => panic!("JsonEncode derive only supports struct-fields.")
            }
        }
        _ => panic!("JsonEncode derive only supports struct item.")
    }
}

#[proc_macro_derive(JsonDecode, attributes(key, ignore))]
pub fn json_decode(input: TokenStream) -> TokenStream {
    let item: Item = json_parse::parse(input).expect("error parsing item");
    match item {
        Item::Struct(s) => {
            match s.fields {
                Fields::StructFields(v) => {
                    let (v, ig): (Vec<_>, Vec<_>) = v.iter().partition(|x| {
                        if let Some(meta) = &x.meta {
                            return !meta.idents().into_iter().any(|x| x == "ignore");
                        }
                        true
                    });
                    let v: Vec<(String, String, String)> = v.iter()
                        .map(|x| {
                            let k = if let Some(meta) = &x.meta {
                                if let Some(key) = meta.get("key") {
                                    key.string().expect("key attribute should be string").to_string()
                                } else {
                                    x.ident.clone()
                                }
                            } else {
                                x.ident.clone()
                            };
                            (format!("\"{}\"", k), x.ident.clone(), x.ty.used_ty())
                        })
                        .collect();
                    quote! {
                        impl#(#(&s.generics))? json::FromJson for #(s.ident)#(#(&s.generics):x #(x.only_idents()))?
                        #(#(&s.where_clause))?
                        {
                            fn from_json(json: &json::Json) -> Result<Self, json::Error> {
                                match json {
                                    json::Json::Object(obj) => {
                                        Ok(
                                            #(s.ident) {
                                                #(#v:x
                                                    #(x.1): {
                                                        if let Some((v, _)) = obj.get(#(x.0)) {
                                                            <#(x.2) as json::FromJson>::from_json(v)?
                                                        } else {
                                                            return Err(json::Error::new(json::ErrorKind::MissingKey,
                                                                                        format!("missing key \"{}\".", #(x.0))));
                                                        }
                                                    },
                                                )*
                                                #(#ig:x
                                                    #(x.ident): std::default::Default::default(),
                                                )*
                                            }
                                        )
                                    }
                                    other => Err(json::Error::new(json::ErrorKind::ConvertError,
                                                                  format!("cannot convert {} to {}.",
                                                                          other._kind(), stringify!(#(s.ident))))),
                                }
                            }
                        }
                    }
                }
                _ => panic!("JsonDecode derive only supports struct-fields.")
            }
        }
        _ => panic!("JsonDecode derive only supports struct item.")
    }
}