use crate::green as g;
use crate::green::NodeOrToken::Token;
use crate::kinds::*;

use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

struct Parser {
    tokens: Peekable<IntoIter<Rc<g::TokenData>>>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Rc<g::TokenData>>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekable(),
            pos: 0,
        }
    }
    pub fn peek(&mut self) -> SyntaxKind {
        self.tokens.peek().map(|token| token.kind()).unwrap_or(EOF)
    }
    pub fn next(&mut self) -> Rc<g::TokenData> {
        self.pos += 1;
        self.tokens.next().unwrap_or(Rc::new(g::TokenData {
            kind: EOF,
            text: "".to_string(),
        }))
    }
}

/// Take a list of tokens and turn them into a green tree.
/// Doesn't handle unmatched parens currently, should only be used in testing.
pub fn parse_exprs(tokens: Vec<Rc<g::TokenData>>) -> g::Node {
    let mut parser = Parser::new(tokens);

    type NodeVec = Vec<g::NodeOrToken<g::Node, g::Token>>;
    let mut stack: NodeVec = Vec::new();

    while parser.peek() != EOF {
        let tok = parser.next();

        match tok.kind {
            // something should be on the stack, doesn't handle case with starting ')'
            R_PAREN => {
                let mut sub_children: NodeVec = Vec::new();
                sub_children.push(tok.clone().into());

                while let Some(token) = stack.pop() {
                    match token {
                        // only token that can trigger new node creation is L_PAREN
                        Token(t) => {
                            if t.kind() == L_PAREN {
                                sub_children.push(t.into());
                                sub_children.reverse();
                                let node =
                                    Rc::new(g::NodeData::new(PAREN_EXPR, sub_children.clone()));
                                stack.push(node.into());
                                break;
                            } else {
                                sub_children.push(t.into());
                            }
                        }
                        n => sub_children.push(n),
                    }
                } // what happens if you never hit a left paren???
            }
            _ => stack.push(tok.clone().into()),
        }
    }
    Rc::new(g::NodeData::new(ROOT, stack))
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::lexer as lex;

    #[test]
    fn can_parse_tokens_into_green_tree() {
        let expr = " (+ 1 (* 2 3)) 1";
        let (leftover, tokens) = lex::lex(expr).unwrap();
        assert!(leftover.is_empty());

        let space = Rc::new(g::TokenData::new(WHITESPACE, " ".to_string()));
        let one = Rc::new(g::TokenData::new(INT, "1".to_string()));
        let two = Rc::new(g::TokenData::new(INT, "2".to_string()));
        let three = Rc::new(g::TokenData::new(INT, "3".to_string()));
        let plus = Rc::new(g::TokenData::new(PLUS, "+".to_string()));
        let star = Rc::new(g::TokenData::new(STAR, "*".to_string()));
        let lp = Rc::new(g::TokenData::new(L_PAREN, "(".to_string()));
        let rp = Rc::new(g::TokenData::new(R_PAREN, ")".to_string()));

        // (* 2 3)
        let inner_expr = Rc::new(g::NodeData::new(
            PAREN_EXPR,
            Vec::from([
                lp.clone().into(),
                star.into(),
                space.clone().into(),
                two.into(),
                space.clone().into(),
                three.into(),
                rp.clone().into(),
            ]),
        ));
        // (+ 1 (* 2 3)) -- inner_expr is shared
        let outer_expr = Rc::new(g::NodeData::new(
            PAREN_EXPR,
            Vec::from([
                lp.into(),
                plus.into(),
                space.clone().into(),
                one.clone().into(),
                space.clone().into(),
                inner_expr.into(),
                rp.into(),
            ]),
        ));

        // whole expr " (+ 1 (* 2 3)) 1"
        let expected_green_tree = Rc::new(g::NodeData::new(
            ROOT,
            Vec::from([
                space.clone().into(),
                outer_expr.into(),
                space.into(),
                one.into(),
            ]),
        ));

        let green_tree = parse_exprs(tokens);
        assert_eq!(expected_green_tree, green_tree);
    }
}
