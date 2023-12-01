extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use std::str::FromStr;
use syn::{parse_macro_input, Ident, LitStr};

#[proc_macro]
pub fn python(input: TokenStream) -> TokenStream {
    let input_str = input.to_string();

    let output = if input_str.starts_with("print") {
        let content = input_str.trim_start_matches("print").trim();
        let content = extract_inside_parentheses(content).unwrap_or("");
        quote! {
            println!(#content);
        }
    } else if input_str.starts_with("for") {
        let parts: Vec<&str> = input_str.split_whitespace().collect();
        let var_name_str = parts[1];
        let var_name = Ident::new(var_name_str, proc_macro2::Span::call_site());

        // Extract numeric part from range(10)
        let range_part = parts[3].trim_start_matches("range(").trim_end_matches(')');
        // Attempt to parse the extracted number as an integer
        match usize::from_str(range_part) {
            Ok(range_end) => quote! {
                for #var_name in 0..#range_end {
                    println!("{}", #var_name);
                }
            },
            Err(_) => {
                println!("Error parsing range part: {}", range_part);
                quote! {}
            }
        }
    } else {
        quote! {}
    };

    output.into()
}

/// Helper function to extract content inside parentheses
fn extract_inside_parentheses(s: &str) -> Option<&str> {
    let start = s.find('(')?;
    let end = s.find(')')?;

    // Also remove any ending quotations if they exist, both double or single quotes
    let start = s[start + 1..]
        .find('"')
        .map(|i| start + i + 1)
        .unwrap_or(start);

    let end = s[..end].rfind('"').unwrap_or(end);

    Some(&s[start + 1..end])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro_functionality() {
    }
}
