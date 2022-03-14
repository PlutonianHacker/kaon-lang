use kaon::common::{Source, Span};
use kaon::error::{Diagnostic, Emitter, Label};

struct MockError;

impl Emitter for MockError {}

#[test]
fn test_one_line() {
    let file_1 = Span::new(
        12,
        10,
        &Source::new(r#"1 muffins + -2 muffins"#, "muffins.lang"),
    );

    let _diagnostic = Diagnostic::error()
        .with_code("E0001")
        .with_message("not enough muffins")
        .with_labels(vec![
            Label::primary(file_1).with_message("cannot have negative muffins")
        ])
        .with_help(vec!["try using bagels instead".to_string()]);

    //MockError.emit(&[diagnostic]);
}

#[test]
fn test_multiline_error() {
    let file_1 = Source::new(
        r#"
entry {
    const x: String = 10
    x += "hello"
}
        "#,
        "hello.lang",
    );

    let _diagnostic = vec![Diagnostic::error()
        .with_code("E0305")
        .with_message("mismatched types")
        .with_labels(vec![
            Label::primary(Span::new(31, 2, &file_1))
                .with_message("expected `String`, found `integer`"),
            Label::primary(Span::new(38, 12, &file_1))
                .with_message("cannot assign value to immutable variable `x`"),
            Label::secondary(Span::new(22, 6, &file_1)).with_message("expected due to this"),
        ])];

    //MockError.emit(&diagnostic);
}

#[test]
fn test_warning() {
    let source = Source::new("var args = Args.new()", "src/test.lang");

    let _warning = Diagnostic::warning()
        .with_message("unused variable `args`")
        .with_labels(vec![Label::primary(Span::new(4, 4, &source)).with_message(
            "help: if this is intentional, prefix it with an underscore: `_args`",
        )])
        .with_help(vec![
            "dead code must be used".to_string(),
            "if this is bothering you
            consider turning off warnings"
                .to_string(),
        ]);

    //MockError.emit(&[warning]);
}
