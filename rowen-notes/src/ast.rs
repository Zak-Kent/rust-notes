// the goal here is to build a strictly typed tree on top of the dynamically typed
// Rowan syntax trees. These wrapper types will wrap RedNodes so they can navigate
// around the tree using the red api

use crate::kinds as k;
use crate::red as r;
use crate::red::RedNode;

#[derive(Debug, PartialEq, Eq)]
enum Symbol {
    Plus,
    Mult,
}

#[derive(Debug, PartialEq, Eq)]
enum Term {
    Sym(Symbol), // symbol like * or +
    Ident,       // fn or var name
    Int,
    Trivia,
}

// TODO: might want to clean up the types for exprs, it's a little clunky
// to have to check if args is_some in order to figure out if you're at
// a node or not

#[derive(Debug)]
struct Expr {
    red_node: RedNode,
    op_typ: Term,
    args: Option<Vec<Expr>>,
}

#[derive(Debug)]
struct ASTExprParseError {
    msg: String,
}

impl Expr {
    fn parse_expr(p_expr: r::RedNode) -> Result<Expr, ASTExprParseError> {
        // match the green node that the red node is wrapping
        match p_expr.green() {
            r::GreenNodeOrToken::Node(n) => {
                if n.kind() != k::PAREN_EXPR {
                    Err(ASTExprParseError {
                        msg: "Expected paren expression".to_owned(),
                    })
                } else {
                    let mut expr_children = p_expr.children();
                    // note .nth consumes the preceding and returned element
                    // remove the L_PAREN and grab op. ex. (* 1 2)
                    // TODO: fix bug here with whitespace before operator. ex. ( * 1 2)
                    let op_typ = match expr_children.nth(1) {
                        Some(c) => match c.kind() {
                            k::INT => Term::Int,
                            k::NAME => Term::Ident,
                            k::PLUS => Term::Sym(Symbol::Plus),
                            k::STAR => Term::Sym(Symbol::Mult),
                            _ => return Err(ASTExprParseError {
                                msg: format!("unsupported token: {:?} found in op place", c.kind()),
                            })
                        },
                        None => {
                            return Err(ASTExprParseError {
                                msg: "Expected token in operator position of paren expr".to_owned(),
                            });
                        }
                    };

                    let args = expr_children.map(|c| Self::parse_expr(c).ok()).collect();

                    Ok(Expr {
                        red_node: p_expr,
                        op_typ,
                        args,
                    })
                }
            }
            r::GreenNodeOrToken::Token(t) => {
                let op_typ = match t.kind() {
                    k::INT => Term::Int,
                    k::NAME => Term::Ident,
                    k::PLUS => Term::Sym(Symbol::Plus),
                    k::STAR => Term::Sym(Symbol::Mult),
                    _ => Term::Trivia, // symbols that don't add meaning to the AST node
                };
                Ok(Expr {
                    red_node: p_expr,
                    op_typ,
                    args: None,
                })
            }
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use super::*;
    use crate::green_node_parser as gp;
    use crate::lexer as lex;
    use crate::red as r;

    #[test]
    fn parse_simple_paren_expr_to_ast(){
        let expr = "(+ 1 2)";
        let (leftover, tokens) = lex::lex(expr).unwrap();
        assert!(leftover.is_empty());
        let mut parser = gp::Parser::new(tokens);
        let green_root = gp::parse_exprs(&mut parser);
        let red_root = r::RedNodeData::new_root(green_root);

        // TODO as you add more parsers make one that handles red roots
        // unwrap the root part of the node
        let red_expr = red_root.children().nth(0).unwrap();

        let ast_expr = Expr::parse_expr(red_expr).unwrap();

        // check round trip from AST node back to string
        assert_eq!(expr, format!("{}", ast_expr.red_node.green()));

        // check arg count which includes trivia. ex. [_ 1 _ 2 )]
        assert_eq!(5, ast_expr.args.unwrap().len())
    }

    #[test]
    fn parse_nested_paren_expr_to_ast() {
        let expr = "(+ 1 2 (* 3 4))";
        let (leftover, tokens) = lex::lex(expr).unwrap();
        assert!(leftover.is_empty());
        let mut parser = gp::Parser::new(tokens);
        let green_root = gp::parse_exprs(&mut parser);
        let red_root = r::RedNodeData::new_root(green_root);
        let red_expr = red_root.children().nth(0).unwrap();
        let ast_expr = Expr::parse_expr(red_expr).unwrap();

        // check round trip from AST node back to string
        assert_eq!(expr, format!("{}", ast_expr.red_node.green()));

        // check arg count which includes trivia. ex. [_ 1 _ 2 _ <mult-node>)]
        assert_eq!(7, ast_expr.args.as_ref().unwrap().len());

        // check arg count which includes trivia. ex. [_ 3 _ 4 )]
        let mult_node = &ast_expr.args.unwrap()[5];
        assert_eq!(5, mult_node.args.as_ref().unwrap().len());
        assert_eq!(Term::Sym(Symbol::Mult), mult_node.op_typ);
    }
}
