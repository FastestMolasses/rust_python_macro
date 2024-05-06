extern crate proc_macro;

mod ast;

use ast::{parse_statement, AstNode, Expression, Identifier, Range};
use nom::IResult;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse::Parse, parse_macro_input};

fn parse_program(input: &str) -> IResult<&str, Vec<AstNode>> {
    nom::multi::many0(parse_statement)(input)
}

/// Custom parser to handle inline Python-like syntax
struct PythonCode {
    code: String,
}

impl Parse for PythonCode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut code = String::new();

        // Collect all tokens into a single string.
        while !input.is_empty() {
            let token: TokenStream2 = input.parse()?;
            code.push_str(&token.to_string());
        }

        Ok(PythonCode { code })
    }
}

#[proc_macro]
pub fn python(input: TokenStream) -> TokenStream {
    let PythonCode { code } = parse_macro_input!(input as PythonCode);
    println!("Input code: {}", code);

    let (remaining, ast) = parse_program(&code).expect("Failed to parse input");
    println!("Parsed AST: {:?}", ast);
    println!("Remaining input: {:?}", remaining);

    let output = generate_rust_code(ast);
    println!("Generated Rust code: {}", output);

    TokenStream::from(output)
}

fn generate_rust_code(ast_nodes: Vec<AstNode>) -> proc_macro2::TokenStream {
    let mut code = quote! {};
    for node in ast_nodes {
        let node_code = match node {
            AstNode::Print(expr) => {
                let rust_expr = convert_expression(expr);
                quote! {
                    println!("{}", #rust_expr);
                }
            }
            AstNode::ForLoop(ident, range, body) => {
                let ident_str = convert_identifier(ident);
                let (start_code, end_code) = convert_range(range);
                let body_code = generate_rust_code(body);
                quote! {
                    for #ident_str in #start_code..#end_code {
                        #body_code
                    }
                }
            }
            _ => quote! {},
        };
        code = quote! {
            #code
            #node_code
        };
    }
    println!("{}", code);
    code
}

fn convert_expression(expr: Expression) -> proc_macro2::TokenStream {
    match expr {
        Expression::Identifier(id) => {
            let ident = Identifier(id);
            convert_identifier(ident)
        }
        Expression::Number(num) => quote! { #num },
        Expression::Range(start, end) => {
            let start_code = convert_expression(*start);
            let end_code = convert_expression(*end);
            quote! { #start_code..#end_code }
        }
        Expression::StringLiteral(s) => quote! { #s },
    }
}

fn convert_identifier(ident: Identifier) -> proc_macro2::TokenStream {
    let Identifier(id) = ident;
    quote! { #id }
}

fn convert_range(range: Range) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let Range(start, end) = range;
    (convert_expression(start), convert_expression(end))
}
