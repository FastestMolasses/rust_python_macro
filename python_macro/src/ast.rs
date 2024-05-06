use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, newline},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use quote::quote;

pub fn parse_statement(input: &str) -> IResult<&str, AstNode> {
    println!("parse_statement input: {:?}", input);
    let result = nom::branch::alt((parse_print, parse_for_loop))(input);
    println!("parse_statement result: {:?}", result);
    result
}

fn lex_identifier(input: &str) -> IResult<&str, Token> {
    map(
        tuple((alpha1, many0(alphanumeric1))),
        |(first, rest): (&str, Vec<&str>)| {
            let mut identifier = first.to_string();
            for r in rest {
                identifier.push_str(r);
            }
            match identifier.as_str() {
                "print" => Token::Print,
                "for" => Token::For,
                "in" => Token::In,
                "range" => Token::Range,
                _ => Token::Identifier(identifier),
            }
        },
    )(input)
}

fn lex_number(input: &str) -> IResult<&str, Token> {
    map(nom::character::complete::i32, Token::Number)(input)
}

fn lex_colon(input: &str) -> IResult<&str, Token> {
    map(char(':'), |_| Token::Colon)(input)
}

fn lex_newline(input: &str) -> IResult<&str, Token> {
    map(char('\n'), |_| Token::Newline)(input)
}

// Combine all lexers into a main lexer function
fn lexer(input: &str) -> IResult<&str, Vec<Token>> {
    many0(nom::branch::alt((
        lex_identifier,
        lex_number,
        lex_colon,
        lex_newline,
    )))(input)
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Print(Expression),
    ForLoop(Identifier, Range, Vec<AstNode>), // for identifier in range: body
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Number(i32),
    Range(Box<Expression>, Box<Expression>),
    StringLiteral(String),
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq)]
pub struct Range(pub Expression, pub Expression);

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Number(i32),
    Print,
    For,
    In,
    Range,
    Colon,
    Newline,
}

fn parse_identifier(input: &str) -> IResult<&str, Expression> {
    println!("parse_identifier input: {:?}", input);
    map(lex_identifier, |token| match token {
        Token::Identifier(name) => Expression::Identifier(name),
        _ => unreachable!(),
    })(input)
}

fn parse_string_literal(input: &str) -> IResult<&str, Expression> {
    let (input, string) = delimited(char('"'), take_until("\""), char('"'))(input)?;
    Ok((input, Expression::StringLiteral(string.to_string())))
}

fn parse_number(input: &str) -> IResult<&str, Expression> {
    println!("parse_number input: {:?}", input);
    map(lex_number, |token| match token {
        Token::Number(value) => Expression::Number(value),
        _ => unreachable!(),
    })(input)
}

fn parse_range(input: &str) -> IResult<&str, Expression> {
    let (input, _) = tag("range")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, start) = parse_expression(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = opt(char(','))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, end) = opt(parse_expression)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(')')(input)?;

    let end = end.unwrap_or(Expression::Number(0));

    Ok((input, Expression::Range(Box::new(start), Box::new(end))))
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    println!("parse_expression input: {:?}", input);
    let result = nom::branch::alt((
        parse_string_literal,
        parse_number,
        parse_identifier,
        parse_range,
    ))(input);
    println!("parse_expression result: {:?}", result);
    result
}

fn parse_print(input: &str) -> IResult<&str, AstNode> {
    println!("parse_print input: {:?}", input);
    let (input, _) = tag("print")(input)?;
    let (input, _) = multispace0(input)?;
    println!("After removing 'print' and space: {:?}", input);

    let (input, expr) = delimited(char('('), parse_expression, char(')'))(input)?;
    println!("Remaining input after parsing expression: {:?}", input);

    let (input, _) = multispace0(input)?;
    let (input, _) = opt(char('\n'))(input)?;

    Ok((input, AstNode::Print(expr)))
}

fn parse_for_loop(input: &str) -> IResult<&str, AstNode> {
    println!("parse_for_loop input: {:?}", input);
    let (input, _) = tag("for")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, identifier) = parse_identifier(input)?;
    let (input, _) = multispace1(input)?;
    let (input, _) = tag("in")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, range) = parse_range(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = many0(preceded(multispace0, parse_statement))(input)?;
    println!("After parsing body: {:?}", input);

    if let Expression::Identifier(id) = identifier {
        if let Expression::Range(start, end) = range {
            return Ok((
                input,
                AstNode::ForLoop(Identifier(id), Range(*start, *end), body),
            ));
        }
    }
    unreachable!()
}
