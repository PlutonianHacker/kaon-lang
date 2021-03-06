use kaon::compiler::{ASTNode, BinExpr, Expr, Stmt, Op, AST, Lexer, Parser};
use kaon::common::{Span, Source};
use kaon::error::Error;

use std::rc::Rc;

fn new_parser(input: &str) -> Result<(AST, Rc<Source>), Error> {
    let source = Source::new(input, "./main");
    let tokens = Lexer::new(source.clone()).tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    Ok((parser.parse().unwrap(), source.clone()))
}

#[test]
fn parse_literal() {
    let (ast, source) = new_parser("7").unwrap();
    assert_eq!(
        ast.nodes[0],
        ASTNode::from(Stmt::Expr(Expr::Number(7.0, Span::new(0, 1, &source))))
    );
}

#[test]
fn parse_bin_expr() {
    let (ast, source) = new_parser("1 + 2").unwrap();
    assert_eq!(
        ast.nodes[0],
        ASTNode::from(Stmt::Expr(Expr::BinExpr(
            Box::new(BinExpr::new(
                Op::Add,
                Expr::Number(1.0, Span::new(0, 1, &source)),
                Expr::Number(2.0, Span::new(4, 1, &source))
            )),
            Span::new(0, 5, &source)
        )))
    )
}

#[test]
fn test_parser() {
    let input = r#"
        1 + 2
        10 - 9
        2 * 3
        8 / 4
    "#;
    let res = new_parser(input);
    assert_eq!(res.is_ok(), true);
}
