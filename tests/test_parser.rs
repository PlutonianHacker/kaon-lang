use kaon_lang::analysis::SemanticAnalyzer;
use kaon_lang::ast::{BinExpr, Expr, Literal, Op};
use kaon_lang::parser::{Parser, ParserRes};
use std::rc::Rc;

fn new_parser(input: String) -> ParserRes {
    let mut analyzer = SemanticAnalyzer::new();
    Parser::new(input).parse(&mut analyzer)
}

#[test]
fn parse_literal() {
    let ast = new_parser("7".to_string());
    assert_eq!(ast.unwrap().nodes[0], Expr::Literal(Literal::Number(7.0)))
}

#[test]
fn parse_bin_expr() {
    let ast = new_parser("1 + 2".to_string());
    assert_eq!(
        ast.unwrap().nodes[0],
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
    let res = new_parser(input);
    assert_eq!(res.is_ok(), true);
}
