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
use crate::kinds::SyntaxKind;

use std::rc::Rc;

type RedNode = Rc<RedNodeData>;
struct RedNodeData {
    parent: Option<RedNode>,
    offset: usize,
    green: GreenNode,
}

impl RedNodeData {
    pub fn new(root: GreenNode) -> RedNode {
        Rc::new(RedNodeData {
            parent: None,
            offset: 0,
            green: root,
        })
    }

    pub fn green(&self) -> &GreenNode {
        &self.green
    }
    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
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
}
