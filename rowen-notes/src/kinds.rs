/*
  The Syntaxkind struct is used to identify the leaf nodes in "green" trees.
*/


#[derive(Copy, Clone, Debug)]
pub struct SyntaxKind(u16);

// Nodes
pub const FN: SyntaxKind = SyntaxKind(1);
pub const PARAM_LIST: SyntaxKind = SyntaxKind(2);
pub const BIN_EXPR: SyntaxKind = SyntaxKind(3);

// Tokens
pub const FN_KW: SyntaxKind = SyntaxKind(100);
pub const NAME: SyntaxKind = SyntaxKind(101);
pub const IDENT: SyntaxKind = SyntaxKind(102);
pub const INT: SyntaxKind = SyntaxKind(103);
pub const PLUS: SyntaxKind = SyntaxKind(104);
pub const WHITESPACE: SyntaxKind = SyntaxKind(105);
pub const STAR: SyntaxKind = SyntaxKind(106);
