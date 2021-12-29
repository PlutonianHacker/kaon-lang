use crate::ast::AST;
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct ParserErr(pub String);

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn consume(&mut self, token_type: TokenType) -> Result<(), ParserErr> {
        match token_type {
            token_type if token_type == self.tokens[self.pos].token_type => {
                self.pos += 1;
                Ok(())
            }
            _ => Err(ParserErr(format!(
                "Parser Error: unexpected token `{:?}`",
                self.tokens[self.pos].token_val
            ))),
        }
    }

    fn file(&mut self) -> Result<Vec<AST>, ParserErr> {
        let mut nodes = vec![];
        while self.tokens[self.pos].token_type != TokenType::Eof {
            if self.tokens[self.pos].token_type == TokenType::Newline {
                self.consume(TokenType::Newline)?;
                continue;
            }
            nodes.push(self.statement()?);
        }

        Ok(nodes)
    }

    fn statement(&mut self) -> Result<AST, ParserErr> {
        match self.tokens[self.pos] {
            _ => self.sum(),
        }
    }

    fn sum(&mut self) -> Result<AST, ParserErr> {
        let mut node = self.term()?;
        loop {
            match &self.tokens.clone()[self.pos].token_type {
                &TokenType::Symbol(ref sym) if sym == "+" => {
                    self.consume(TokenType::Symbol(sym.to_string()))?;
                    node = AST::func_call("add", vec![node, self.term()?]);
                }
                &TokenType::Symbol(ref sym) if sym == "-" => {
                    self.consume(TokenType::Symbol(sym.to_string()))?;
                    node = AST::func_call("sub", vec![node, self.term()?]);
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn term(&mut self) -> Result<AST, ParserErr> {
        let mut node = self.literal()?;
        loop {
            match &self.tokens.clone()[self.pos].token_type {
                &TokenType::Symbol(ref sym) if sym == "*" => {
                    self.consume(TokenType::Symbol(sym.to_string()))?;
                    node = AST::func_call("mul", vec![node, self.literal()?]);
                }
                &TokenType::Symbol(ref sym) if sym == "/" => {
                    self.consume(TokenType::Symbol(sym.to_string()))?;
                    node = AST::func_call("div", vec![node, self.literal()?]);
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn literal(&mut self) -> Result<AST, ParserErr> {
        match &self.tokens.clone()[self.pos].token_type {
            &TokenType::Number => {
                self.consume(TokenType::Number)?;
                Ok(AST::number(
                    self.tokens[self.pos - 1].token_val.parse::<f64>().unwrap(),
                ))
            }
            &TokenType::String => {
                self.consume(TokenType::String)?;
                Ok(AST::string(&self.tokens[self.pos - 1].token_val))
            }
            &TokenType::Keyword(ref sym) if sym == "true" || sym == "false" => {
                self.consume(TokenType::Keyword(sym.to_string()))?;
                Ok(AST::boolean(sym.clone().parse::<bool>().unwrap()))
            }
            _ => Err(ParserErr(format!(
                "Parser Error: unexpected token `{}`",
                self.tokens[self.pos].token_val
            ))),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AST>, ParserErr> {
        self.file()
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::parser::AST;
    use crate::source::Source;
    use std::path::PathBuf;

    #[test]
    fn test_parser() {
        let source = Source::new("1 + 2", &PathBuf::from("./main"));
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer.tokenize().unwrap());
        let ast = parser.parse().unwrap();

        assert_eq!(
            ast[0],
            AST::FuncCall("add".to_string(), vec![AST::Number(1.0), AST::Number(2.0)])
        );
    }
}
