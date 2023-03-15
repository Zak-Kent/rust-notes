/*
  The Syntaxkind struct is used to identify the nodes in "green" trees.
*/


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SyntaxKind(u16);

// Nodes
pub const ROOT: SyntaxKind = SyntaxKind(0);
pub const FN: SyntaxKind = SyntaxKind(1);
pub const PARAM_LIST: SyntaxKind = SyntaxKind(2);
pub const BIN_EXPR: SyntaxKind = SyntaxKind(3);
pub const PAREN_EXPR: SyntaxKind = SyntaxKind(4);

// Tokens
pub const FN_KW: SyntaxKind = SyntaxKind(100);
pub const NAME: SyntaxKind = SyntaxKind(101);
pub const IDENT: SyntaxKind = SyntaxKind(102);
pub const INT: SyntaxKind = SyntaxKind(103);
pub const PLUS: SyntaxKind = SyntaxKind(104);
pub const WHITESPACE: SyntaxKind = SyntaxKind(105);
pub const STAR: SyntaxKind = SyntaxKind(106);
pub const L_PAREN: SyntaxKind = SyntaxKind(107);
pub const R_PAREN: SyntaxKind = SyntaxKind(108);
