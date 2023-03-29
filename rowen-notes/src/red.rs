/*
 Goals for red nodes (cursors):
   - in green nodes you don't know the offset of string in the text, due
   to the structural sharing and reuse of nodes via Rc you don't have a
   way to know which piece of text in the original maps to which node
   - In a red node you want to track:
   - .parent() -> have a method that when called returns a parent node
   - .text_offset() -> offset to start of node in original text

   - in GreenNodes reference counted pointers to children are stored, in
   RedNodes it's the reverse where Rc pointers to parents are stored. This
   allows you traverse the tree in both directions
*/

use crate::green::{Node as GreenNode, NodeOrToken, Token as GreenToken};
use crate::green_node_parser as gp;
use crate::kinds::SyntaxKind;

use core::fmt;
use std::iter;
use std::rc::Rc;

/*
This enum is used to wrap the two types of green nodes that could be
found in a red tree (GreenNode | GreenToken). Doing this lets RedNode
handle both types and reduces duplication.
 */
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GreenNodeOrToken<N, T> {
    Node(N),
    Token(T),
}

impl From<GreenToken> for GreenNodeOrToken<GreenNode, GreenToken> {
    fn from(v: GreenToken) -> Self {
        Self::Token(v)
    }
}

impl From<GreenNode> for GreenNodeOrToken<GreenNode, GreenToken> {
    fn from(v: GreenNode) -> Self {
        Self::Node(v)
    }
}

impl fmt::Display for GreenNodeOrToken<GreenNode, GreenToken> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GreenNodeOrToken::Node(n) => fmt::Display::fmt(n, f),
            GreenNodeOrToken::Token(t) => fmt::Display::fmt(t, f),
        }
    }
}

type GreenNorT = GreenNodeOrToken<GreenNode, GreenToken>;

pub type RedNode = Rc<RedNodeData>;
#[derive(Debug, PartialEq, Eq)]
pub struct RedNodeData {
    parent: Option<RedNode>,
    idx: Option<usize>, // index in parent's green.children
    offset: usize,
    green: GreenNorT,
}

impl RedNodeData {
    pub fn new_root(root: GreenNode) -> RedNode {
        Rc::new(RedNodeData {
            parent: None,
            idx: None,
            offset: 0,
            green: root.into(),
        })
    }

    pub fn green(&self) -> &GreenNorT {
        &self.green
    }
    pub fn kind(&self) -> SyntaxKind {
        match self.green() {
            GreenNodeOrToken::Node(node) => node.kind(),
            GreenNodeOrToken::Token(token) => token.kind(),
        }
    }
    pub fn text_len(&self) -> usize {
        match self.green() {
            GreenNodeOrToken::Node(node) => node.text_len(),
            GreenNodeOrToken::Token(token) => token.text_len(),
        }
    }
    pub fn parent(&self) -> Option<&RedNode> {
        self.parent.as_ref()
    }
    pub fn text_offset(&self) -> usize {
        self.offset
    }

    // the Box<dyn ... > is needed here because the iterator returned will
    // return different types when it hits a node vs. token and using impl,
    // static dispatch doesn't allow for this because the return types are
    // different
    pub fn children<'a>(self: &'a RedNode) -> Box<dyn Iterator<Item = RedNode> + 'a> {
        let node = match self.green() {
            GreenNodeOrToken::Node(node) => node,
            // tokens don't have children, so they return empty iterator when hit
            GreenNodeOrToken::Token(_token) => return Box::new(iter::empty()),
        };

        /*
        because we need to compute the offset_in_parent while we step through
        children, any access to children is in linear time O(N). If we store
        the offset_in_parent value for each child in the underlying
        GreenNodeData.children vector using a pair (offset_in_parent, child)
        you can then get an nth_child method that's O(1) avoiding the need to
        keep track of the offset while iterating the children. This also allows
        for a child_containting_range method that's O(log(N)) to find a child in
        a range.

        These methods are used in Rust-Analyzer to support pointers which are
        either a path into a syntax tree or an offset where a child node lives
        in the original text. These pointers allow Rust-Analyzer to pass
        information about syntax nodes and how then change accross time. You can
        save a pointer to a node and then don't have to keep the RedNode
        structure for that node in memory, because the RedNode can be rebuilt
        when needed from the green tree using the pointer
         */
        let mut offset_in_parent = 0;

        Box::new(node.children().enumerate().map(move |(idx, green_child)| {
            let offset = offset_in_parent + self.text_offset();
            offset_in_parent += green_child.text_len();

            // match here is for NodeOrToken inside green nodes
            match green_child {
                NodeOrToken::Node(node) => Rc::new(RedNodeData {
                    parent: Some(Rc::clone(self)),
                    idx: Some(idx),
                    offset,
                    green: node.into(),
                }),
                NodeOrToken::Token(token) => Rc::new(RedNodeData {
                    parent: Some(Rc::clone(self)),
                    idx: Some(idx),
                    offset,
                    green: token.into(),
                }),
            }
        }))
    }

    fn walk_up_tree_and_replace_nodes(self: &RedNode, new_green: GreenNode) -> RedNode {
        match self.parent() {
            // if there is a parent the idx value must exist, .unwrap() is safe
            Some(parent) => {
                match parent.replace_child(self.idx.unwrap(), new_green) {
                    Some(red_node) => red_node,
                    // When walking the tree upward it shouldn't be possible for replace_child
                    // to hit a token on the way up after the first call, it should be nodes
                    // the whole way to root. The extra unwrapping here is clumsy but necessary
                    // for RedNodeData to handle both tokens and nodes wrapped in GreenNodeOrToken
                    None => panic!("replace_child call hit a token on the way up :("),
                }
            }
            None => RedNodeData::new_root(new_green),
        }
    }

    /// Replace children in the underlying green tree via the RedNode API
    /// Option is returned to prevent this method from being called on a GreenTokens
    /// The returned RedNode will be the root RedNode for the updated tree
    pub fn replace_child(self: &RedNode, idx: usize, new_green: GreenNode) -> Option<RedNode> {
        let node = match self.green() {
            GreenNodeOrToken::Node(node) => node,
            // tokens don't have children, so they aren't allowed to replace them
            GreenNodeOrToken::Token(_token) => return None,
        };

        let new_g_node = node.replace_child(idx, new_green.into());
        Some(self.walk_up_tree_and_replace_nodes(new_g_node.into()))
    }
}

#[cfg(test)]
mod red_node_tests {
    use super::*;
    use crate::green_node_parser as gp;
    use crate::lexer as lex;

    #[test]
    fn can_wrap_a_red_root_around_green() {
        let expr = " (+ 1 (* 2 3)) 1";
        let (leftover, tokens) = lex::lex(expr).unwrap();
        assert!(leftover.is_empty());
        let mut parser = gp::Parser::new(tokens);

        let green_root = gp::parse_exprs(&mut parser);
        let red_root = RedNodeData::new_root(green_root);

        let root_children: Vec<RedNode> = red_root.children().collect();

        // there should be a total of 4 children from the root of the expr above
        assert_eq!(root_children.len(), 4);

        // token children return empty iter, only inner expr: (+ 1 (* 2 3)) will
        // return kinds and inner (* 2 3) counts as one node
        let paren_expr_child_count = root_children
            .iter()
            .flat_map(|child| {
                child
                    .children()
                    .map(|ic| ic.kind())
                    .collect::<Vec<SyntaxKind>>()
            })
            .count();
        assert_eq!(7, paren_expr_child_count);
    }

    #[test]
    fn can_use_red_api_to_navigate_tree() {
        let expr = " (+ 1 (* 2 3)) 1";
        let (leftover, tokens) = lex::lex(expr).unwrap();
        assert!(leftover.is_empty());
        let mut parser = gp::Parser::new(tokens);

        let green_root = gp::parse_exprs(&mut parser);
        let red_root = RedNodeData::new_root(green_root);

        // 2nd child of red_root = (+ 1 (* 2 3))
        // 6th child of expr above = (* 2 3)
        // 4th child of expr above = 2
        let two = red_root
            .children().nth(1).unwrap()
            .children().nth(5).unwrap()
            .children().nth(3).unwrap();

        // defer to green nodes impl of display
        assert_eq!("2", format!("{}", two.green()));
        assert_eq!(9, two.text_offset());

        let mult_expr = two.parent().unwrap();
        assert_eq!(
            "(* 2 3)",
            // this shows navigation from child to parent via the red node api
            format!("{}", mult_expr.green())
        );
        assert_eq!(6, mult_expr.text_offset());
    }

    #[test]
    fn can_navigate_tree_and_replace_a_node() {
        let expr = "(+ 1 2 (* 3 4))";
        let (leftover, tokens) = lex::lex(expr).unwrap();
        assert!(leftover.is_empty());
        let mut parser = gp::Parser::new(tokens);
        let green_root = gp::parse_exprs(&mut parser);
        let red_root = RedNodeData::new_root(green_root);

        // construct a replacement GreenNode
        let replacement = "(* 5 6)";
        let (leftover, r_tokens) = lex::lex(replacement).unwrap();
        assert!(leftover.is_empty());
        let mut r_parser = gp::Parser::new(r_tokens);
        let green_replacement = gp::parse_exprs(&mut r_parser);

        // 0th child of expr = "(+ 1 2 (* 3 4))"
        let child_expr = red_root.children().nth(0).unwrap();

        // 5th child of child_expr = 2
        let fifth_child = child_expr.children().nth(5).unwrap();
        assert_eq!("2", format!("{}", fifth_child.green()));

        // 2 in the original expr should be replaced with (* 5 6)
        let replaced_expr = child_expr.replace_child(5, green_replacement).unwrap();
        assert_eq!("(+ 1 (* 5 6) (* 3 4))",
                   format!("{}", replaced_expr.clone().green()));

        // notice a new root RedNode is returned from replace_child
        let fifth_child_of_replaced = replaced_expr
            // need to unwrap outer root node
            .children().nth(0).unwrap()
             // 5th child of replaced_expr = (* 5 6)
            .children().nth(5).unwrap();
        assert_eq!("(* 5 6)", format!("{}", fifth_child_of_replaced.green()));
    }
}
