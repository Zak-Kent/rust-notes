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

use std::iter;
use std::rc::Rc;

// re-using NodeOrToken here to wrap the green nodes stored in RedNodeData
// to avoid needing duplicated RedNodeData/RedTokenData structs
type GreenNodeOrToken = NodeOrToken<GreenNode, GreenToken>;

pub type RedNode = Rc<RedNodeData>;
#[derive(Debug, PartialEq, Eq)]
struct RedNodeData {
    parent: Option<RedNode>,
    offset: usize,
    green: GreenNodeOrToken,
}

impl RedNodeData {
    pub fn new(root: GreenNode) -> RedNode {
        Rc::new(RedNodeData {
            parent: None,
            offset: 0,
            green: root.into(),
        })
    }

    pub fn green(&self) -> &GreenNodeOrToken {
        &self.green
    }
    pub fn kind(&self) -> SyntaxKind {
        match self.green() {
            NodeOrToken::Node(node) => node.kind(),
            NodeOrToken::Token(token) => token.kind(),
        }
    }
    pub fn text_len(&self) -> usize {
        self.green().text_len()
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
            NodeOrToken::Node(node) => node,
            // tokens don't have children, so they return empty iterator when hit
            NodeOrToken::Token(_token) => return Box::new(iter::empty()),
        };

        let mut offset_in_parent = 0;

        Box::new(node.children().iter().map(move |green_child| {
            let offset = offset_in_parent + self.text_offset();
            offset_in_parent += green_child.text_len();

            match green_child {
                NodeOrToken::Node(node) => Rc::new(RedNodeData {
                    parent: Some(Rc::clone(self)),
                    offset,
                    green: Rc::clone(node).into(),
                }),
                NodeOrToken::Token(token) => Rc::new(RedNodeData {
                    parent: Some(Rc::clone(self)),
                    offset,
                    green: Rc::clone(token).into(),
                }),
            }
        }))
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

        let green_root = gp::parse_exprs(tokens);
        let red_root = RedNodeData::new(green_root);

        let root_children: Vec<RedNode> = red_root.children().collect();

        // there should be a total of 4 children from the root of the expr above
        assert_eq!(root_children.len(), 4);

        // token children return empty iter, only inner expr: (+ 1 (* 2 3)) will
        // return kinds and inner (* 2 3) counts as one node
        let paren_expr_child_count = root_children.iter().flat_map(|child| {
            child.children().map(|ic| ic.kind()).collect::<Vec<SyntaxKind>>()
        })
        .count();
        assert_eq!(7, paren_expr_child_count);
    }
}
