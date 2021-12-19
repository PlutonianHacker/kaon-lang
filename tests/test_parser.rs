use kaon_lang::ast::{BinExpr, Expr, Literal, Op};
use kaon_lang::parser::Parser;
use std::rc::Rc;

#[test]
fn parse_literal() {
    let mut parser = Parser::new("7".to_string());
    let ast = parser.parse();
    assert_eq!(ast.nodes[0], Expr::Literal(Literal::Number(7.0)))
}

#[test]
fn parse_bin_expr() {
    #![allow(irrefutable_let_patterns)]
    let mut parser = Parser::new("1 + 2".to_string());
    let ast = parser.parse();
    assert_eq!(
        ast.nodes[0],
        Expr::BinExpr(Rc::new(BinExpr {
            op: Op::Add,
            lhs: Expr::Literal(Literal::Number(1.0)),
            rhs: Expr::Literal(Literal::Number(2.0)),
        }))
    )
}

#[test]
fn test_parser() {
    let input = r#"
        1 + 2
        10 - 9
        2 * 3
        8 / 4
    "#
    .to_string();
    let mut parser = Parser::new(input);
    let _ = parser.parse();
}
