use crate::kinds::*;
use crate::green::*;

use nom::{
    branch::alt,
    character::complete::{alpha1, digit1, space1},
    bytes::complete::tag,
    multi::many1,
    IResult,
};
use std::rc::Rc;

fn ws(input: &str) -> IResult<&str, TokenData> {
    let (input, ws) = space1(input)?;
    Ok((input, TokenData::new(WHITESPACE, ws.to_string())))
}

fn int(input: &str) -> IResult<&str, TokenData> {
    let (input, int) = digit1(input)?;
    Ok((input, TokenData::new(INT, int.to_string())))
}

fn ident(input: &str) -> IResult<&str, TokenData> {
    let (input, ident) = alpha1(input)?;
    Ok((input, TokenData::new(IDENT, ident.to_string())))
}

/*
  Indirection with Box needed when returning closures from a func because the
  compiler doesn't know what size the closure is going to be
  https://doc.rust-lang.org/book/ch19-05-advanced-functions-and-closures.html
 */
fn tok_tag(tk: &str, sk: SyntaxKind) -> Box<dyn Fn(&str) -> IResult<&str, TokenData> + '_> {
    Box::new(move |input| {
        let (input, token): (&str, &str) = tag(tk)(input)?;
        Ok((input, TokenData::new(sk, token.to_string())))
    })
}

fn choose_token(input: &str) -> IResult<&str, Token> {
    let (input, token) = alt((tok_tag("+", PLUS),
                              tok_tag("*", STAR),
                              tok_tag("(", L_PAREN),
                              tok_tag(")", R_PAREN),
                              int,
                              ws,
                              ident,
    ))(input)?;
    // wrapping tokens in Rc to match what's expected in green tree
    Ok((input, Rc::new(token)))
}

fn lex(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, tokens) = many1(choose_token)(input)?;
    assert!(input.is_empty());
    Ok((input, tokens))
}


#[cfg(test)]
mod tokenize_tests {
    use super::*;

    #[test]
    fn can_handle_single_token() {
        let tok = "+";
        if let Ok((leftover, p)) = choose_token(tok){
            assert!(leftover.len() == 0);
            assert_eq!(p, Rc::new(TokenData::new(PLUS, "+".to_string())))
        } else {
            panic!("parse failed")
        }
    }

    #[test]
    fn can_tokenize_expr() {
        let expr = "(+  1 23 foo)";
        if let Ok((leftover, parsed_expr)) = lex(expr){
            assert!(leftover.is_empty());
            assert_eq!(parsed_expr, Vec::from(
                [Rc::new(TokenData::new(L_PAREN, "(".to_string())),
                 Rc::new(TokenData::new(PLUS, "+".to_string())),
                 Rc::new(TokenData::new(WHITESPACE, "  ".to_string())), // notice the double space
                 Rc::new(TokenData::new(INT, "1".to_string())),
                 Rc::new(TokenData::new(WHITESPACE, " ".to_string())),
                 Rc::new(TokenData::new(INT, "23".to_string())),
                 Rc::new(TokenData::new(WHITESPACE, " ".to_string())),
                 Rc::new(TokenData::new(IDENT, "foo".to_string())),
                 Rc::new(TokenData::new(R_PAREN, ")".to_string())),
                ]))
        } else {
            panic!("parse failed")
        }
    }
}
