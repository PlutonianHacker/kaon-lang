use crate::common::Span;

use std::fmt::{self, Display};

pub const KEYWORDS: &[&str] = &[
    "and", "or", "if", "else", "var", "con", "loop", "while", "for", "in", "break", "continue",
    "fun", "return", "class", "const", "self", "import", "from", "public",
];

/// Represents a keyword.
#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    /// +
    Plus,
    /// -
    Hypen,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Modulo,
    /// ~
    Tilde,
    /// :
    Colon,
    /// ,
    Comma,
    /// .
    Dot,
    /// ...
    DotDotDot,
    /// &
    And,
    /// |
    Or,
    /// ^
    Xor,
    /// =
    Equal,
    /// !
    Bang,
    /// ?
    QuestionMark,
    /// >
    RightAngleBracket,
    /// <
    LeftAngleBracket,
    /// ==
    EqualsEquals,
    /// !=
    NotEqual,
    /// >=
    GreaterThanEqual,
    /// <=
    LessThanEqual,
    /// =>
    Arrow,
    /// <<
    LeftShift,
    /// >>
    RightShift,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => f.write_str("+"),
            Self::Hypen => f.write_str("-"),
            Self::Star => f.write_str("*"),
            Self::Slash => f.write_str("\\"),
            Self::Modulo => f.write_str("%"),
            Self::Tilde => f.write_str("~"),
            Self::Colon => f.write_str(":"),
            Self::Comma => f.write_str(","),
            Self::Dot => f.write_str("."),
            Self::And => f.write_str("&"),
            Self::Or => f.write_str("|"),
            Self::Equal => f.write_str("="),
            Self::Bang => f.write_str("!"),
            Self::QuestionMark => f.write_str("?"),
            Self::RightAngleBracket => f.write_str(">"),
            Self::LeftAngleBracket => f.write_str("<"),
            Self::Xor => f.write_str("^"),
            Self::EqualsEquals => f.write_str("=="),
            Self::NotEqual => f.write_str("!="),
            Self::GreaterThanEqual => f.write_str(">="),
            Self::LessThanEqual => f.write_str("<="),
            Self::Arrow => f.write_str("=>"),
            Self::LeftShift => f.write_str("<<"),
            Self::RightShift => f.write_str(">>"),
            Self::DotDotDot => f.write_str("..."),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
/// An opening or closing delimiter.
pub enum Delimiter {
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// \n
    Newline,
    /// <eof>
    Eof,
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Delimiter::OpenBrace => f.write_str("{"),
            Delimiter::CloseBrace => f.write_str("}"),
            Delimiter::OpenBracket => f.write_str("["),
            Delimiter::CloseBracket => f.write_str("]"),
            Delimiter::OpenParen => f.write_str("("),
            Delimiter::CloseParen => f.write_str(")"),
            Delimiter::Newline => f.write_str("\\n"),
            Delimiter::Eof => f.write_str("<eof>"),
        }
    }
}

/// A comment token.
#[derive(Debug, Clone, PartialEq)]
pub enum Comment {
    /// //
    LineComment,
    /// /*
    BlockComment,
    /// ///
    LineDocComment,
    /// /**
    BlockDocComment,
}

impl Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Comment::LineComment => f.write_str("//"),
            Comment::BlockComment => f.write_str("/*"),
            Comment::LineDocComment => f.write_str("///"),
            Comment::BlockDocComment => f.write_str("/**"),
        }
    }
}

/// A keyword.
#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    /// var
    Var,
    /// final
    Final,
    /// and
    And,
    /// or
    Or,
    /// if
    If,
    /// else
    Else,
    /// loop
    Loop,
    /// while
    While,
    /// for
    For,
    /// in
    In,
    /// break
    Break,
    /// continue
    Continue,
    /// fun
    Fun,
    /// return
    Return,
    /// class
    Class,
    /// const
    Const,
    /// this
    This,
    /// import
    Import,
    /// from
    From,
    /// public
    Public,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Var => f.write_str("var"),
            Keyword::Final => f.write_str("final"),
            Keyword::And => f.write_str("and"),
            Keyword::Or => f.write_str("or"),
            Keyword::If => f.write_str("if"),
            Keyword::Else => f.write_str("else"),
            Keyword::Loop => f.write_str("loop"),
            Keyword::While => f.write_str("while"),
            Keyword::For => f.write_str("for"),
            Keyword::In => f.write_str("in"),
            Keyword::Break => f.write_str("break"),
            Keyword::Continue => f.write_str("continue"),
            Keyword::Fun => f.write_str("fun"),
            Keyword::Return => f.write_str("return"),
            Keyword::Class => f.write_str("class"),
            Keyword::Const => f.write_str("const"),
            Keyword::This => f.write_str("this"),
            Keyword::Import => f.write_str("import"),
            Keyword::From => f.write_str("from"),
            Keyword::Public => f.write_str("public"),
        }
    }
}

/// A literal value.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// number literal
    NumberLiteral(String),
    /// string literal
    StringLiteral(String),
    /// char literal
    CharLiteral(String),
    /// Identifier
    Id(String),
    /// boolean true
    True,
    /// boolean false
    False,
    /// nil literal
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::NumberLiteral(number) => f.write_fmt(format_args!("{number}")),
            Literal::StringLiteral(string) => f.write_fmt(format_args!("{string}")),
            Literal::CharLiteral(char) => f.write_fmt(format_args!("{char}")),
            Literal::Id(id) => f.write_fmt(format_args!("{id}")),
            Literal::True => f.write_str("true"),
            Literal::False => f.write_str("false"),
            Literal::Nil => f.write_str("nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    /// a keyword
    Keyword(Keyword),
    /// a symbol
    Symbol(Symbol),
    /// a comment
    Comment(Comment),
    /// An opening or closing delimiter.
    Delimiter(Delimiter),
    /// A literal token.
    Literal(Literal),
}

impl TokenType {
    pub fn symbol(sym: &str) -> TokenType {
        let sym = match sym {
            "+" => Symbol::Plus,
            "-" => Symbol::Hypen,
            "*" => Symbol::Star,
            "/" => Symbol::Slash,
            "%" => Symbol::Modulo,
            "~" => Symbol::Tilde,
            ":" => Symbol::Colon,
            "," => Symbol::Comma,
            "." => Symbol::Dot,
            "&" => Symbol::And,
            "|" => Symbol::Or,
            "^" => Symbol::Xor,
            "=" => Symbol::Equal,
            "!" => Symbol::Bang,
            "?" => Symbol::QuestionMark,
            ">" => Symbol::RightAngleBracket,
            "<" => Symbol::LeftAngleBracket,
            "==" => Symbol::EqualsEquals,
            "!=" => Symbol::NotEqual,
            ">=" => Symbol::GreaterThanEqual,
            "<=" => Symbol::LessThanEqual,
            "=>" => Symbol::Arrow,
            "<<" => Symbol::LeftShift,
            ">>" => Symbol::RightShift,
            "..." => Symbol::DotDotDot,
            sym => unimplemented!("{sym}")
        };

        TokenType::Symbol(sym)
    }

    pub fn keyword(keyword: &str) -> TokenType {
        match keyword {
            "or" => TokenType::Keyword(Keyword::Or),
            "in" => TokenType::Keyword(Keyword::In),
            "if" => TokenType::Keyword(Keyword::If),
            "var" => TokenType::Keyword(Keyword::Var),
            "and" => TokenType::Keyword(Keyword::And),
            "for" => TokenType::Keyword(Keyword::For),
            "fun" => TokenType::Keyword(Keyword::Fun),
            "else" => TokenType::Keyword(Keyword::Else),
            "loop" => TokenType::Keyword(Keyword::Loop),
            "this" => TokenType::Keyword(Keyword::This),
            "from" => TokenType::Keyword(Keyword::From),
            "break" => TokenType::Keyword(Keyword::Break),
            "final" => TokenType::Keyword(Keyword::Final),
            "while" => TokenType::Keyword(Keyword::While),
            "class" => TokenType::Keyword(Keyword::Class),
            "const" => TokenType::Keyword(Keyword::Const),
            "return" => TokenType::Keyword(Keyword::Return),
            "import" => TokenType::Keyword(Keyword::Import),
            "public" => TokenType::Keyword(Keyword::Public),
            "continue" => TokenType::Keyword(Keyword::Continue),
            _ => unimplemented!(),
        }
    }

    pub fn comment(typ: &str) -> TokenType {
        match typ {
            "//" => TokenType::Comment(Comment::LineComment),
            "/*" => TokenType::Comment(Comment::BlockComment),
            "///" => TokenType::Comment(Comment::LineDocComment),
            "/**" => TokenType::Comment(Comment::BlockDocComment),
            _ => unimplemented!(),
        }
    }

    pub fn delimiter(typ: &str) -> TokenType {
        match typ {
            "{" => TokenType::Delimiter(Delimiter::OpenBrace),
            "}" => TokenType::Delimiter(Delimiter::CloseBrace),
            "[" => TokenType::Delimiter(Delimiter::OpenBracket),
            "]" => TokenType::Delimiter(Delimiter::CloseBracket),
            "(" => TokenType::Delimiter(Delimiter::OpenParen),
            ")" => TokenType::Delimiter(Delimiter::CloseParen),
            "\\n" => TokenType::Delimiter(Delimiter::Newline),
            "<eof>" => TokenType::Delimiter(Delimiter::Eof),
            sym => unimplemented!("{sym}"),
        }
    }

    pub fn eof() -> TokenType {
        TokenType::Delimiter(Delimiter::Eof)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &*self {
            TokenType::Keyword(keyword) => write!(f, "{keyword}"),
            TokenType::Symbol(symbol) => write!(f, "{symbol}"),
            TokenType::Comment(typ) => write!(f, "{typ}"),
            TokenType::Delimiter(delimiter) => write!(f, "{delimiter}"),
            TokenType::Literal(literal) => write!(f, "{literal}"),
        }
    }
}

pub type Token = (TokenType, Span);
