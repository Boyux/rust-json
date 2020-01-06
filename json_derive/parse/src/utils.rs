use std::iter::FromIterator;
use proc_macro::{TokenTree, TokenStream};

// punct_eq generate a closure to judge whether a TokenTree is
// the given punct, which usually use in iter.filter() method.
pub fn punct_eq(c: char) -> impl FnMut(&TokenTree) -> bool {
    move |x: &TokenTree| {
        if let TokenTree::Punct(p) = x {
            return p.as_char() == c;
        }
        false
    }
}

// split the given TokenStream to a Vec<TokenStream> by char, usually
// use to split generic parameters because rust does not treated angle
// bracket as a valid group delimiter. It is preferred to use split_tokens_by
// than slice.split when input is elegantly split by separator, and every
// scope has valid element. This function will stop collecting when the
// last token is the separator so it wouldn't contain an empty TokenStream at end.
pub fn split_tokens_by(input: TokenStream, sep: &str) -> Vec<TokenStream> {
    // manually split generics by punct sep
    // HINT: because there are nested generics parameters, so we
    // can't split by sep directly on stream slice, we should
    // make sure split with sep just in the most outside scope.
    // look like: "T: SomeTrait<R, P>, U"
    let mut tokens: Vec<TokenStream> = Vec::new();

    // let's process, there are two loop scopes
    let mut iter = input
        .into_iter()
        .peekable();
    let mut n = 0;

    // the outer loop is for collecting all the tokens in generic,
    // split by sep, look the same as slice.split, but it handles
    // sep inside a generic("<...>"), which was not treated as a
    // separator.
    loop {
        let mut inner = Vec::new();

        // the inner loop is for collection tokens before a sep separator,
        // when it hit the separator, it breaks. Only when n == 0, a sep
        // was treated as a separator.
        loop {
            match iter.next() {
                Some(next) => {
                    // case #1: '<' -> n++
                    // case #2: '>' -> n--
                    match &next {
                        // HINT: '>' after '-' is not case #2
                        TokenTree::Punct(punct) if punct.as_char() == '-' => {
                            match iter.peek() {
                                Some(TokenTree::Punct(peek)) if peek.as_char() == '>' => {
                                    inner.push(next);
                                    inner.push(iter.next().unwrap());
                                    continue;
                                }
                                _ => ()
                            }
                        }

                        // case #1
                        TokenTree::Punct(punct) if punct.as_char() == '<' => n += 1,
                        // case #2
                        TokenTree::Punct(punct) if punct.as_char() == '>' => n -= 1,

                        // hit single sep
                        TokenTree::Punct(punct) if n == 0 && sep.len() == 1 => {
                            if punct.as_char() == sep.chars().next().unwrap() {
                                break;
                            }
                        }
                        // hit multi-sep
                        // HINT: multiple separators should be check one by one,
                        TokenTree::Punct(punct) if n == 0 => {
                            let mut chars = sep.chars();
                            // check if current punct match the first sep char
                            if punct.as_char() == chars.next().unwrap() {
                                // hit represents if left sep matched
                                let hit = chars
                                    .zip(iter.clone())
                                    .all(|(c, x)| {
                                        match x {
                                            TokenTree::Punct(ref punct) if punct.as_char() == c => true,
                                            _ => false
                                        }
                                    });
                                if hit {
                                    // because the first punct is already consume, so
                                    // we should only skip length of sep - 1 steps. It
                                    // should be sep.len() - 2 when using iter.nth()
                                    iter.nth(sep.len() - 2);
                                    break;
                                }
                            }
                        }

                        _ => ()
                    }

                    inner.push(next);
                }

                // end of tokens, break
                None => break
            }
        }

        tokens.push(TokenStream::from_iter(inner));

        // break when no elements in iter
        if iter.peek().is_none() {
            break;
        }
    }

    // return valid tokens
    tokens
}