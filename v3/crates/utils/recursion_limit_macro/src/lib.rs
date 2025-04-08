extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{ItemFn, ReturnType, parse_macro_input, spanned::Spanned};

/// A helper attribute for limiting recursion to avoid stack overflows. Annotate any or all
/// functions involved in recursion. Each entry of such a function will decrement `self`'s
/// `recursion_limit` counter, returning an `Err` when we exhaust the limit.
///
/// It assumes:
///   - the annotated function returns a `Result`
///   - `self` is mutable
///   - ... has a numeric `recursion_limit` property initialized to 1 or higher
///   - ... has a method `error_str<T,Foo>(error_msg: &'static str) -> Result<Foo,T>`
#[proc_macro_attribute]
pub fn limit_recursion(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);

    // Check return type
    let return_type_check = if let ReturnType::Type(_, type_path) = &input_fn.sig.output {
        let type_str = quote! {#type_path}.to_string();
        if type_str.contains("Result") {
            None
        } else {
            Some(quote_spanned! { type_path.span() =>
                compile_error!("`#[limit_recursion]` can only be used on functions that return `Result`.");
            })
        }
    } else {
        Some(quote_spanned! { input_fn.sig.output.span() =>
            compile_error!("`#[limit_recursion]` can only be used on functions that return `Result`.");
        })
    };

    // Extract parts of the function
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = input_fn;

    // Generate the new function code with error checks included
    let output = quote! {
        #(#attrs)* #vis #sig {
            #return_type_check
            self.recursion_limit -= 1;
            if self.recursion_limit <= 0 {
                self.error_str("Recursion limit exceeded")?
            }
            let result = (|| #block)();
            self.recursion_limit += 1;
            result
        }
    };

    TokenStream::from(output)
}
