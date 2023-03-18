/*
  The "green" tree in this file is a typical immutable syntax tree which
  supports cheap updates through the use of Rc. Instead of making deep copies
  of sub trees when replacing a child, Rc shares references to the sub trees
  and avoids the cost of a deep copy.

  Traversal of "green" trees can only be done top down because the NodeData
  struct only has access to its children and not its parent.
*/

use crate::kinds::*;
use crate::lexer as lex;

use core::fmt;
use std::fmt::format;
use std::iter;
use std::rc::Rc;

/*
  This enum is used to wrap the two types that you could find in the children
  vector of a given node in the tree, the children can be other Nodes or leaf
  Tokens
*/
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NodeOrToken<N, T> {
    Node(N),
    Token(T),
}

impl NodeOrToken<Node, Token> {
    /*
     text_len is needed later on when the offset of elements in the original
     text is wanted. The red/green tree structure allows access to any char in
     the target text file.
    */
    pub fn text_len(&self) -> usize {
        match self {
            NodeOrToken::Node(n) => n.text_len(),
            NodeOrToken::Token(t) => t.text_len(),
        }
    }
}

impl From<Token> for NodeOrToken<Node, Token> {
    fn from(v: Token) -> Self {
        Self::Token(v)
    }
}

impl From<Node> for NodeOrToken<Node, Token> {
    fn from(v: Node) -> Self {
        Self::Node(v)
    }
}

impl fmt::Display for NodeOrToken<Node, Token> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeOrToken::Node(n) => fmt::Display::fmt(n, f),
            NodeOrToken::Token(t) => fmt::Display::fmt(t, f),
        }
    }
}

// wrapping TokenData and NodeData in Rc allows for cheap structural sharing
// later on when modifying the children of a node
pub type Token = Rc<TokenData>;
// a leaf node in the tree
#[derive(Debug, PartialEq, Eq)]
pub struct TokenData {
    pub kind: SyntaxKind,
    pub text: String,
}

impl TokenData {
    pub fn new(kind: SyntaxKind, text: String) -> TokenData {
        TokenData { kind, text }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text(&self) -> &str {
        self.text.as_str()
    }

    pub fn text_len(&self) -> usize {
        self.text.len()
    }
}

impl fmt::Display for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.text(), f)
    }
}

// Internal nodes in the green tree
pub type Node = Rc<NodeData>;
#[derive(Debug, PartialEq, Eq)]
pub struct NodeData {
    kind: SyntaxKind,
    children: Vec<NodeOrToken<Node, Token>>,
    len: usize,
}

impl NodeData {
    pub fn new(kind: SyntaxKind, children: Vec<NodeOrToken<Node, Token>>) -> NodeData {
        let len = children.iter().map(|e| e.text_len()).sum();
        NodeData {
            kind,
            children,
            len,
        }
    }
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text_len(&self) -> usize {
        self.len
    }

    pub fn children(&self) -> impl Iterator<Item = NodeOrToken<Node, Token>> + '_ {
        // Does this end up being an Rc::clone()? both Node and Token are Rc but
        // does the cloned call see through the enum and inc the Rc counter on the
        // wrapped Node/Token?
        self.children.iter().cloned()
    }

    /// returns a new node with a child replaced in its children vector
    pub fn replace_child(&self, idx: usize, replacement: NodeOrToken<Node, Token>) -> NodeData {
        // the stuff below looks complicated at first but it's just making
        // a full copy of the existing children while dropping out the elm
        // at the idx you want to replace and adding in the replacement
        assert!(idx < self.children.len());
        let new_children: Vec<_> = self
            .children()
            .take(idx) // takes elms up to idx return as iter
            // iter::once used to put replacement in an iter
            .chain(iter::once(replacement.clone())) // chain two iters together
            // chain the first elms + replacement iter with everything
            // after the replaced child. (idx + 1) makes sure everything
            // before was dropped
            .chain(self.children().skip(idx + 1))
            .collect();

        // written another way that's more clear
        let left_children = self.children().take(idx);
        let right_children = self.children().skip(idx + 1);
        let other_new_children: Vec<_> = left_children
            .chain(iter::once(replacement))
            .chain(right_children)
            .collect();

        // the way this is written above is costly because the cloned() calls
        // above are going to make a deep clone of the whole sub tree which
        // isn't as fast as it could be if structural sharing it used avoiding
        // the need to make deep copies
        NodeData::new(self.kind, new_children)
    }
}

impl fmt::Display for NodeData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for child in self.children() {
            fmt::Display::fmt(&child, f)?;
        }
        Ok(())
    }
}

#[test]
fn can_build_and_print_green_tree() {
    // 1 + 2
    let ws = Rc::new(TokenData::new(WHITESPACE, " ".to_string()));
    let one = Rc::new(TokenData::new(INT, "1".to_string()));
    let two = Rc::new(TokenData::new(INT, "2".to_string()));
    let plus = Rc::new(TokenData::new(PLUS, "+".to_string()));

    let addition_node = Rc::new(NodeData::new(
        BIN_EXPR,
        vec![
            /*
             The clone calls below are Rc clone calls which are bumping the
             strong count for each Rc<node> and allowing sharing of nodes in the
             green tree.

             The .into() calls are wrapping Rc<NodeData> and Rc<TokenData> in
             the NodeOrToken enum using the From<Node>/From<Token> impl for
             NodeOrToken defined above.
            */
            one.into(),
            ws.clone().into(),
            plus.into(),
            ws.clone().into(),
            two.into(),
        ],
    ));

    // TODO: update this test when a tokenizer is added

    let add_expr = "1 + 2";
    let add_expr_tree_as_str = format!("{}", addition_node);
    // check that you can take a tree and flip it back to a String loss free
    assert_eq!(add_expr, &add_expr_tree_as_str);
}
