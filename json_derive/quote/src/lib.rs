/// quote provides a shortcut to quickly generate TokenStream from hand-writing
/// code with local variables catching support. You can use #var or #(#var)* to
/// use var's (iter)value in quote macro:
///
/// # Examples
///
/// ```
/// extern crate proc_macro;
///
/// use proc_macro::TokenStream;
///
/// let name = "say_hello";
/// let args = quote!(name: &str, age: i32);
/// let body = "println!(\"my name is {}, age {}\", name, age);";
///
/// let token: TokenStream = quote! {
///     pub fn #name(#(#args)*) {
///         #body
///     }
/// };
///
/// let say_hello: TokenStream = "pub fn say_hello(name: &str, age: i32)
/// { println!(\"my name is {}, age {}\", name, age); }".parse().unwrap();
///
/// assert_eq!(say_hello.to_string(), token.to_string());
/// ```
///
/// # Syntax
///
/// 1. #var: var should implement ToString trait, it converts var to TokenStream.
/// 2. #(#vis:pub)+/-: vis is a boolean value, and '+' means when vis is ture, then
///    converts pub to token else empty, '-' the opposite.
/// 3. #(#opt)?: opt should be Option<T> where T: ToString, it converts opt
///    to TokenStream while None represents empty token.
/// 4. #(#opt:x k = #x)?: opt should be Option<T> where T: ToString, it converts opt
///    to TokenStream with given tokens, while None represents empty token.
/// 5. #(#iter)*: iter should implement IntoIterator trait, it expands values in
///    iter and converts them to TokenStream.
/// 6. #(#iter:x pub #x)*: iter should implement IntoIterator trait, it expands
///    values in iter and map then with `|x| quote!(pub #x)` closure, then collect
///    them in a single TokenStream.
/// 7. #(expr): expr is an expression, it converts expr's value to TokenStream, which
///    should implement ToString trait.
/// 8. #(@modifier: ...): the convert behaviour is different depends on the modifier.
///
/// ```
/// let var = "var";
/// let vis = true;
/// let opt = Some("opt");
/// let iter = vec!["foo", "bar"];
/// let expr = &iter;
///
/// assert_eq!(quote!(#var).to_string(), quote!(var).to_string());
/// assert_eq!(quote!(#(#vis:pub)+).to_string(), quote!(pub).to_string());
/// assert_eq!(quote!(#(#vis:pub)-).to_string(), quote!().to_string());
/// assert_eq!(quote!(#(#opt)?).to_string(), quote!(opt).to_string());
/// assert_eq!(quote!(#(#opt:x k = #x)?).to_string(), quote!(k = opt).to_string());
/// assert_eq!(quote!(#(#iter)*).to_string(), quote!(foo bar).to_string());
/// assert_eq!(quote!(#(#iter:x pub #x),*).to_string(), quote!(pub foo, pub bar).to_string());
/// assert_eq!(quote!(#(expr[0])).to_string(), quote!(foo).to_string());
/// ```
///
/// # Recursion Limit
///
/// quote use a large amount of recursive call to expand vars and generate TokenStream,
/// but the rust complier limit the recursion in 32 times by default, so when you want
/// a complex code including multiple #var you might increase the recursion limit by
/// adding #![recursion_limit = "128"] to your crate(or much higher).
#[macro_export(local_inner_macros)]
macro_rules! quote {
    ( @options($($option:ident$(($($params:tt)+))?),+): $($tt:tt)* ) => {{
        let mut _token = $crate::__pm::TokenStream::new();
        let _span = $crate::__pm::Span::call_site();
        __parse_options!((_token _span $($tt)*) $($option$(($($params)+))?)+);
        _token
    }};
    ( @span($span:expr): $($tt:tt)* ) => {{
        let mut _token = $crate::__pm::TokenStream::new();
        let _span = $span;
        __append_to_token!(_token _span $($tt)*);
        _token
    }};
    ( @literal: $($tt:tt)* ) => {{
        let mut _token = $crate::__pm::TokenStream::new();
        let _span = $crate::__pm::Span::call_site();
        __append_to_token!(_token _span #(@literal:$($tt)*));
        _token
    }};
    ( @params($($params:tt)*): $($tt:tt)* ) => {{
        let mut _token = $crate::__pm::TokenStream::new();
        let _span = $crate::__pm::Span::call_site();
        __append_to_token!(_token _span #(@params($($params)*):$($tt)*));
        _token
    }};
    ( @scope: $($tt:tt)* ) => {{
        let mut _token = $crate::__pm::TokenStream::new();
        let _span = $crate::__pm::Span::call_site();
        __append_to_token!(_token _span #(@scope:$($tt)*));
        _token
    }};
    ( $($tt:tt)* ) => {{
        let mut _token = $crate::__pm::TokenStream::new();
        let _span = $crate::__pm::Span::call_site();
        __append_to_token!(_token _span $($tt)*);
        _token
    }};
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __parse_options {
    ( ($_token:ident $_span:ident $($tt:tt)*) $(@$flag:ident$(($($args:tt)*))?)? span($span:expr) $($option:ident$(($($params:tt)+))?)* ) => {
        let $_span = $span;
        __parse_options!(($_token $_span $($tt)*) $(@$flag$(($($args)*))?)? $($option$(($($params)+))?)*);
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) $(@$flag:ident$(($($args:tt)*))?)? literal $($option:ident$(($($params:tt)+))?)* ) => {
        __parse_options!(($_token $_span $($tt)*) @literal $($option$(($($params)+))?)*);
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) $(@$flag:ident$(($($args:tt)*))?)? params($($inner:tt)*) $($option:ident$(($($params:tt)+))?)* ) => {
        __parse_options!(($_token $_span $($tt)*) @params($($inner)*) $($option$(($($params)+))?)*);
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) $(@$flag:ident$(($($args:tt)*))?)? scope $($option:ident$(($($params:tt)+))?)* ) => {
        __parse_options!(($_token $_span $($tt)*) @scope $($option$(($($params)+))?)*);
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) @literal ) => {
        __append_to_token!($_token $_span #(@literal:$($tt)*));
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) @params($($inner:tt)*) ) => {
        __append_to_token!($_token $_span #(@params($($inner)*):$($tt)*));
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) @scope ) => {
        __append_to_token!($_token $_span #(@scope:$($tt)*));
    };
    ( ($_token:ident $_span:ident $($tt:tt)*) ) => {
        __append_to_token!($_token $_span $($tt)*);
    };
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __scope_command {
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident {$($tt:tt)*} ) => {
        if $flag.0 {
            let input = $crate::__parse(__stringify!($($tt)*), $span);
            if let Some(ref vec) = $iter.0 {
                let tmp_v = $hashmap.remove($iter.1);
                let k = $iter.1.to_string();
                for e in vec {
                    $hashmap.insert(k.clone(), e.clone());
                    $token.extend($crate::__translate(input.clone(), $span, &$hashmap));
                }
                if let Some(v) = tmp_v {
                    $hashmap.insert(k.clone(), v);
                } else {
                    $hashmap.remove($iter.1);
                }
            } else {
                $token.extend($crate::__translate(input, $span, &$hashmap));
            }
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident define($param:ident($($arg:ident),*), $($quote:tt)+) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #define in #foreach scope.");
        }
        if $flag.0 {
            $(let $arg = __stringify!(#$arg);)*
            let $param = quote!(@span($span):$($quote)+);
            let _param_str = __stringify!($param).to_string();
            let mut _k = _param_str.clone();
            _k.push('+');
            $hashmap.insert(_k, $param);
            let mut _k = _param_str.clone();
            _k.push('-');
            let _v = quote!(@span($span):$($arg),*);
            $hashmap.insert(_k, _v);
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident define($param:ident, $($quote:tt)+) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #define in #foreach scope.");
        }
        if $flag.0 {
            let $param = quote!(@span($span):$($quote)+);
            $hashmap.insert(__stringify!($param).to_string(), $param);
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident define($param:ident) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #define in #foreach scope.");
        }
        if $flag.0 {
            $hashmap.insert(__stringify!($param).to_string(), $crate::__pm::TokenStream::new());
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident include($expr:expr) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #include in #foreach scope.");
        }
        if $flag.0 {
            $token.extend($crate::__translate($expr.clone(), $span, &$hashmap));
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident quote($($tt:tt)*) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #quote in #foreach scope.");
        }
        if $flag.0 {
            $token.extend($crate::__translate(quote!($($tt)*), $span, &$hashmap));
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident execute($stmt:stmt) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #execute in #foreach scope.");
        }
        if $flag.0 {
            $stmt;
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident undef($param:ident) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #undef in #foreach scope.");
        }
        if $flag.0 {
            $hashmap.remove(__stringify!($param));
        }
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident ifdef($param:ident) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #ifdef in #foreach scope.");
        }
        $flag.1 = Some(());
        $flag.0 = $hashmap.contains_key(__stringify!($param));
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident ifndef($param:ident) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #ifndef in #foreach scope.");
        }
        $flag.1 = Some(());
        $flag.0 = !$hashmap.contains_key(__stringify!($param));
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident if($expr:expr) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #if in #foreach scope.");
        }
        $flag.1 = Some(());
        $flag.0 = $expr;
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident elif($expr:expr) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #elif in #foreach scope.");
        }
        $flag.1.expect("expect #if before #elif.");
        $flag.0 = $expr;
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident else ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #else in #foreach scope.");
        }
        $flag.1.expect("expect #if before #else.");
        $flag.0 = !$flag.0;
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident endif ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected #endif in #foreach scope.");
        }
        $flag.1.expect("expect #if before #endif.");
        $flag.1 = None;
        $flag.0 = true;
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident foreach($expr:expr, $x:ident) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected nested #foreach.");
        }
        $iter.0 = Some(
            $expr.into_iter()
                .map(|x| $crate::__parse(&x.to_string(), $span))
                .collect::<Vec<$crate::__pm::TokenStream>>()
        );
        $iter.1 = __stringify!($x);
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident foreach($expr:expr) ) => {
        if $iter.0.is_some() {
            let e: Option<()> = None;
            e.expect("unexpected nested #foreach.");
        }
        let mut _sized_vec = Vec::with_capacity($expr.into_iter().count());
        _sized_vec.resize(_sized_vec.capacity(), TokenStream::new());
        $iter.0 = Some(_sized_vec);
        $iter.1 = "_";
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident endfor ) => {
        $iter.0.expect("expect #foreach before #endfor.");
        $iter.0 = None;
        $iter.1 = "";
    };
    ( $token:ident $span:ident $flag:ident $iter:ident $hashmap:ident end ) => {
        drop($hashmap);
        drop($iter);
        drop($flag);
    };
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __append_to_token {
    // ...
    // ======================================================================
    // parse pounded parenthesis with modifier: #(@modifier(args)?: ...)
    // ======================================================================
    // @literal modifier converts the given tokens to TokenStream directly
    ( $token:ident $span:ident $($ident:ident)* # ( @ literal : $($tt:tt)+ ) $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $($token.extend($crate::__parse(__stringify!($tt), $span));)+
        __append_to_token!($token $span $($rest)*);
    }};
    // @params modifier uses the given variable's value as token
    ( $token:ident $span:ident $($ident:ident)* # ( @ params($($param:ident$(=$expr:expr)?),+$(,)?) : $($tt:tt)+ ) $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        let mut _hashmap = std::collections::HashMap::new();
        $(
        $(let $param = $expr;)?
        _hashmap.insert(__stringify!($param).to_string(), $crate::__parse(&$param.to_string(), $span));
        )+
        let _input = $crate::__parse(__stringify!($($tt)+), $span);
        $token.extend($crate::__translate(_input, $span, &_hashmap));
        __append_to_token!($token $span $($rest)*);
    }};
    // @scope modifier create an isolated scope, which include some useful behaviours
    // #define: define variables or macro by quoting given token
    // #include: include TokenStream to current scope
    // #quote: quote given token by quote! macro
    // #execute: execute rust code in scope
    // #undef: un-define variables
    // #ifdef: if define
    // #ifndef: if not define
    // #if: receive a boolean expression as flag
    // #elif: else if
    // #else: else
    // #endif: set the flag to true
    // #foreach: loop and set variable for each element
    // #endfor: end loop
    // #end: represent the end of scope, any items after #end would cause a compile time error
    // HINT: it looks like preprocessor in C, actually it does the same thing
    ( $token:ident $span:ident $($ident:ident)*
        # ( @ scope :
            $( # $prefix_options:ident$(($($prefix_params:tt)*))? )*
            $({ $($tt:tt)* } $( # $suffix_options:ident$(($($suffix_params:tt)*))? )*)+
          )
      $($rest:tt)* ) =>
    {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        struct _Flag(bool, Option<()>);
        struct _Iter(Option<Vec<$crate::__pm::TokenStream>>, &'static str);
        let mut _flag = _Flag(true, None);
        let mut _iter = _Iter(None, "");
        let mut _hashmap = std::collections::HashMap::new();
        $(__scope_command!($token $span _flag _iter _hashmap $prefix_options$(($($prefix_params)*))?);)*
        $(
        __scope_command!($token $span _flag _iter _hashmap {$($tt)*});
        $(__scope_command!($token $span _flag _iter _hashmap $suffix_options$(($($suffix_params)*))?);)*
        )+
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // parse pounded bool vars: #(#var:...)+/-, '+' is true and '-' is false
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident:$($tt:tt)+ ) + $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        if $tag {
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):$($tt:tt)+ ) + $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        if $expr {
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident:$($tt:tt)+ ) - $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        if !$tag {
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):$($tt:tt)+ ) - $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        if !$expr {
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // parse pounded option var: #(#var)?
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident ) ? $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        match $tag {
            Some(s) => $token.extend($crate::__parse(&s.to_string(), $span)),
            None => ()
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr) ) ? $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        match $expr {
            Some(s) => $token.extend($crate::__parse(&s.to_string(), $span)),
            None => ()
        }
        __append_to_token!($token $span $($rest)*);
    }};
    // parse pounded option var with extra tokens: #(#var:x ...)?
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident:$x:ident $($tt:tt)+ ) ? $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        match $tag {
            Some($x) => __append_to_token!($token $span $($tt)+),
            None => ()
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):$x:ident $($tt:tt)+ ) ? $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        match $expr {
            Some($x) => __append_to_token!($token $span $($tt)+),
            None => ()
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident:_ $($tt:tt)+ ) ? $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        match $tag {
            Some(_) => __append_to_token!($token $span $($tt)+),
            None => ()
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):_ $($tt:tt)+ ) ? $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        match $expr {
            Some(_) => __append_to_token!($token $span $($tt)+),
            None => ()
        }
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // parse pounded iter vars: #(#tags)*
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $tag.into_iter()
            .for_each(|x| {
                $token.extend($crate::__parse(&x.to_string(), $span));
            });
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr) ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $expr.into_iter()
             .for_each(|x| {
                $token.extend($crate::__parse(&x.to_string(), $span));
             });
        __append_to_token!($token $span $($rest)*);
    }};
    // parse pounded iter vars: #(#tags),*
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident ) $sep:tt * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for (_i, _e) in $tag.into_iter().enumerate() {
            if _i > 0 {
                __append_to_token!($token $span $sep);
            }
            $token.extend($crate::__parse(&_e.to_string(), $span));
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr) ) $sep:tt * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for (_i, _e) in $expr.into_iter().enumerate() {
            if _i > 0 {
                __append_to_token!($token $span $sep);
            }
            $token.extend($crate::__parse(&_e.to_string(), $span));
        }
        __append_to_token!($token $span $($rest)*);
    }};
    // parse pounded iter vars: #(#tags,)*
    ( $token:ident $span:ident $($ident:ident)* # ( # $tag:ident $sep:tt ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for _e in $tag.into_iter() {
            $token.extend($crate::__parse(&_e.to_string(), $span));
            __append_to_token!($token $span $sep);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr) $sep:tt ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for _e in $expr.into_iter() {
            $token.extend($crate::__parse(&_e.to_string(), $span));
            __append_to_token!($token $span $sep);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    // parse pounded iter vars with extra tokens: #(#tags:tag ...)*
    ( $token:ident $span:ident $($ident:ident)* # ( # $iter:ident:$x:ident $($tt:tt)+ ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $iter.into_iter()
             .for_each(|$x| __append_to_token!($token $span $($tt)+));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):$x:ident $($tt:tt)+ ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $expr.into_iter()
             .for_each(|$x| __append_to_token!($token $span $($tt)+));
        __append_to_token!($token $span $($rest)*);
    }};
    // parse pounded iter vars with extra tokens: #(#tags:tag ...),*
    ( $token:ident $span:ident $($ident:ident)* # ( # $iter:ident:$x:ident $($tt:tt)+ ) $sep:tt * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for (_i, $x) in $iter.into_iter().enumerate() {
            if _i > 0 {
                __append_to_token!($token $span $sep);
            }
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):$x:ident $($tt:tt)+ ) $sep:tt * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for (_i, $x) in $expr.into_iter().enumerate() {
            if _i > 0 {
                __append_to_token!($token $span $sep);
            }
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # $iter:ident:_ $($tt:tt)+ ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $iter.into_iter()
             .for_each(|_| __append_to_token!($token $span $($tt)+));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):_ $($tt:tt)+ ) * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $expr.into_iter()
             .for_each(|_| __append_to_token!($token $span $($tt)+));
        __append_to_token!($token $span $($rest)*);
    }};
    // parse pounded iter vars with extra tokens: #(#tags:tag ...),*
    ( $token:ident $span:ident $($ident:ident)* # ( # $iter:ident:_ $($tt:tt)+ ) $sep:tt * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for (_i, _) in $iter.into_iter().enumerate() {
            if _i > 0 {
                __append_to_token!($token $span $sep);
            }
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # ( # ($expr:expr):_ $($tt:tt)+ ) $sep:tt * $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        for (_i, _) in $expr.into_iter().enumerate() {
            if _i > 0 {
                __append_to_token!($token $span $sep);
            }
            __append_to_token!($token $span $($tt)+);
        }
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // parse expr surrounded by an pounded parenthesis: #(expr)
    ( $token:ident $span:ident $($ident:ident)* # ( $expr:expr ) $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__parse(&$expr.to_string(), $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // parse pounded var: #tag
    ( $token:ident $span:ident $($ident:ident)* # $tag:ident $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__parse(&$tag.to_string(), $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // double # means a single #
    ( $token:ident $span:ident $($ident:ident)* ## $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('#', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // parse symbols

    // [#![], #[]]
    ( $token:ident $span:ident $($ident:ident)* #![ $($tt:tt)* ] $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('#', $span));
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Bracket,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* #[ $($tt:tt)* ] $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('#', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Bracket,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    // [!(); !{}; ![];]
    ( $token:ident $span:ident $($ident:ident)* ! ( $($tt:tt)* ) ; $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Parenthesis,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ! { $($tt:tt)* } ; $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Brace,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ! [ $($tt:tt)* ] ; $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Bracket,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // [!() !{} ![]]
    ( $token:ident $span:ident $($ident:ident)* ! ( $($tt:tt)* ) $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Parenthesis,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ! { $($tt:tt)* } $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Brace,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ! [ $($tt:tt)* ] $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Bracket,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    // [(); {}; [];]
    ( $token:ident $span:ident $($ident:ident)* ( $($tt:tt)* ); $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Parenthesis,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* { $($tt:tt)* }; $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Brace,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* [ $($tt:tt)* ]; $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Bracket,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // [() {} []]
    ( $token:ident $span:ident $($ident:ident)* ( $($tt:tt)* ) $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Parenthesis,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* { $($tt:tt)* } $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Brace,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* [ $($tt:tt)* ] $($rest:tt)* ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend({
            let mut _g = $crate::__pm::Group::new(
                $crate::__pm::Delimiter::Bracket,
                quote!(@span($span): $($tt)*));
            _g.set_span($span);
            Some($crate::__pm::TokenTree::from(_g))
        });
        __append_to_token!($token $span $($rest)*);
    }};
    // [-> => <= >= != && || == += -= *= /= %= &= ^= |= >> << >>= <<= .. ... ..= ::]
    ( $token:ident $span:ident $($ident:ident)* -> $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('-', '>', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* => $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('=', '>', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* <= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('<', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* >= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('>', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* != $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('!', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* && $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('&', '&', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* || $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('|', '|', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* == $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('=', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* += $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('+', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* -= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('-', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* *= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('*', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* /= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('/', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* %= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('%', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* &= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('&', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ^= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('^', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* |= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('|', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* >> $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('>', '>', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* << $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('<', '<', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* >>= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct3('>', '>', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* <<= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct3('<', '<', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* .. $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2('.', '.', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ... $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct3('.', '.', '.', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ..= $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct3('.', '.', '=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* :: $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct2(':', ':', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // [& : + - * / = > < . , ! ? @ ^ | % ; #]
    ( $token:ident $span:ident $($ident:ident)* & $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('&', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* : $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct(':', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* + $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('+', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* - $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('-', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* * $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('*', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* / $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('/', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* = $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('=', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* > $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('>', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* < $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('<', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* . $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('.', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* , $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct(',', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ! $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('!', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ? $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('?', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* @ $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('@', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ^ $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('^', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* | $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('|', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* % $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('%', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* ; $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct(';', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    ( $token:ident $span:ident $($ident:ident)* # $($rest:tt)*) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)*
        $token.extend($crate::__gen_punct('#', $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // ======================================================================
    // common case
    ( $token:ident $span:ident $($ident:ident)+ ) => {{
        $($token.extend($crate::__parse(__stringify!($ident), $span));)+
    }};
    ( $token:ident $span:ident $first:tt $($rest:tt)* ) => {{
        $token.extend($crate::__parse(__stringify!($first), $span));
        __append_to_token!($token $span $($rest)*);
    }};
    // ======================================================================

    // final case
    ( $token:ident $span:ident ) => {{
        ()
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __stringify {
    ( $($tt:tt)* ) => {
        stringify!($($tt)*)
    };
}

extern crate proc_macro;

pub use proc_macro as __pm;
use proc_macro::{TokenStream, TokenTree, Punct, Spacing, Ident, Group, Span, Delimiter};
use std::collections::HashMap;

pub fn __gen_punct(c: char, span: Span) -> TokenStream {
    let mut punct = Punct::new(c, Spacing::Alone);
    punct.set_span(span);
    TokenTree::Punct(punct).into()
}

pub fn __gen_punct2(c1: char, c2: char, span: Span) -> TokenStream {
    let mut token = TokenStream::new();
    let mut punct = Punct::new(c1, Spacing::Joint);
    punct.set_span(span);
    token.extend(Some(TokenTree::Punct(punct)));
    let mut punct = Punct::new(c2, Spacing::Alone);
    punct.set_span(span);
    token.extend(Some(TokenTree::Punct(punct)));
    token
}

pub fn __gen_punct3(c1: char, c2: char, c3: char, span: Span) -> TokenStream {
    let mut token = TokenStream::new();
    let mut punct = Punct::new(c1, Spacing::Joint);
    punct.set_span(span);
    token.extend(Some(TokenTree::Punct(punct)));
    let mut punct = Punct::new(c2, Spacing::Joint);
    punct.set_span(span);
    token.extend(Some(TokenTree::Punct(punct)));
    let mut punct = Punct::new(c3, Spacing::Alone);
    punct.set_span(span);
    token.extend(Some(TokenTree::Punct(punct)));
    token
}

const IDENT_TABLE: [u8; 63] = [
    b'_', b'a', b'b', b'c', b'd', b'e', b'f',
    b'g', b'h', b'i', b'j', b'k', b'l', b'm',
    b'n', b'o', b'p', b'q', b'r', b's', b't',
    b'u', b'v', b'w', b'x', b'y', b'z', b'A',
    b'B', b'C', b'D', b'E', b'F', b'G', b'H',
    b'I', b'J', b'K', b'L', b'M', b'N', b'O',
    b'P', b'Q', b'R', b'S', b'T', b'U', b'V',
    b'W', b'X', b'Y', b'Z', b'0', b'1', b'2',
    b'3', b'4', b'5', b'6', b'7', b'8', b'9'
];

fn is_ident(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut iter = s.bytes();

    let first = iter.next().unwrap();
    if !IDENT_TABLE[0..53].contains(&first) {
        return false;
    }

    iter.all(|x| IDENT_TABLE.contains(&x))
}

pub fn __parse(s: &str, span: Span) -> TokenStream {
    if is_ident(s) {
        TokenTree::Ident(Ident::new(s, span)).into()
    } else {
        let token: TokenStream = s.parse().unwrap();
        token.into_iter()
            .map(|mut x| {
                x.set_span(span);
                x
            })
            .collect()
    }
}

pub fn __translate(input: TokenStream, span: Span, map: &HashMap<String, TokenStream>) -> TokenStream {
    let mut tokens: Vec<TokenTree> = Vec::new();
    let mut iter = input.into_iter().peekable();
    loop {
        match iter.next() {
            Some(TokenTree::Punct(ref punct)) if punct.as_char() == '#' => {
                match iter.peek() {
                    Some(TokenTree::Ident(ident)) => {
                        if let Some(v) = map.get(&ident.to_string()) {
                            tokens.extend(v.clone());
                        }
                        iter.nth(0);
                    }
                    Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                        let mut inner = group.stream().into_iter();
                        match (inner.next(), inner.next(), inner.next()) {
                            (
                                Some(TokenTree::Ident(ref ident)),
                                Some(TokenTree::Group(ref group)),
                                None
                            ) if group.delimiter() == Delimiter::Parenthesis => {
                                let k = format!("{}-", ident.to_string());
                                if let Some(vs) = map.get(&k) {
                                    let args = split_tokens_by(vs.clone(), ",");
                                    let acts = split_tokens_by(group.stream(), ",");
                                    assert_eq!(args.len(), acts.len());
                                    let mut inner_map = map.clone();
                                    for (k, v) in args.into_iter().zip(acts.into_iter()) {
                                        let v = __translate(v, span, map);
                                        inner_map.insert(k.to_string(), v);
                                    }
                                    let k = format!("{}+", ident.to_string());
                                    if let Some(v) = map.get(&k) {
                                        tokens.extend(__translate(v.clone(), span, &inner_map));
                                    }
                                }
                            }
                            _ => ()
                        }
                        iter.nth(0);
                    }
                    _ => {
                        tokens.extend(__gen_punct('#', span));
                    }
                }
            }
            Some(TokenTree::Group(ref group)) => {
                tokens.extend({
                    let mut _g = Group::new(
                        group.delimiter(),
                        __translate(group.stream(), span, map));
                    _g.set_span(span);
                    Some(TokenTree::from(_g))
                });
            }
            Some(other) => {
                tokens.extend(Some(other));
            }
            None => break,
        }
    }

    let remain = tokens.iter()
        .zip(tokens.iter().skip(1))
        .any(|(x, y)| {
            match (x, y) {
                (TokenTree::Punct(punct), TokenTree::Ident(_)) if punct.as_char() == '#' => true,
                (TokenTree::Punct(punct), TokenTree::Group(group))
                if punct.as_char() == '#' && group.delimiter() == Delimiter::Parenthesis => true,
                _ => false
            }
        });

    if remain {
        __translate(tokens.into_iter().collect(), span, map)
    } else {
        tokens.into_iter()
            .map(|mut x| {
                x.set_span(span);
                x
            })
            .collect()
    }
}

fn split_tokens_by(input: TokenStream, sep: &str) -> Vec<TokenStream> {
    let mut tokens: Vec<TokenStream> = Vec::new();

    let mut iter = input
        .into_iter()
        .peekable();
    let mut n = 0;

    loop {
        let mut inner = Vec::new();

        loop {
            match iter.next() {
                Some(next) => {
                    match &next {
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

                        TokenTree::Punct(punct) if punct.as_char() == '<' => n += 1,
                        TokenTree::Punct(punct) if punct.as_char() == '>' => n -= 1,

                        TokenTree::Punct(punct) if n == 0 && sep.len() == 1 => {
                            if punct.as_char() == sep.chars().next().unwrap() {
                                break;
                            }
                        }
                        TokenTree::Punct(punct) if n == 0 => {
                            let mut chars = sep.chars();
                            if punct.as_char() == chars.next().unwrap() {
                                let hit = chars
                                    .zip(iter.clone())
                                    .all(|(c, x)| {
                                        match x {
                                            TokenTree::Punct(ref punct) if punct.as_char() == c => true,
                                            _ => false
                                        }
                                    });
                                if hit {
                                    iter.nth(sep.len() - 2);
                                    break;
                                }
                            }
                        }

                        _ => ()
                    }

                    inner.push(next);
                }

                None => break
            }
        }

        tokens.push(inner.into_iter().collect::<TokenStream>());

        if iter.peek().is_none() {
            break;
        }
    }

    tokens
}