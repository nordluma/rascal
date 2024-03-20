//! # Rascal
//!
//! Rascal is a Pascal like interpreted language.
//!
//! # Grammar
//!
//! ```no_run
//! program : compound_statement DOT
//!
//! compound_statement : BEGIN statement_list END
//!
//! statement_list : statement
//!                | statement SEMI statement_list
//!
//! statement : compound_statement
//!           | assignment_statement
//!           | empty
//!
//! assignment_statement : variable ASSIGN expr
//!
//! empty :
//!
//! epxr : term ((PLUS | MINUS) term)*
//!
//! term : factor ((MUL | DIV) factor)*
//!
//! factor : PLUS factor
//!        | MINUS factor
//!        | INTEGER
//!        | LPAREN expr RPAREN
//!        | variable
//!
//! variable : ID
//! ```

use std::collections::HashMap;

use once_cell::sync::Lazy;

static RESERVED_KEYWORDS: Lazy<HashMap<&str, Token>> = Lazy::new(|| {
    HashMap::from([
        ("BEGIN", Token::new(TokenType::Begin, "BEGIN")),
        ("END", Token::new(TokenType::End, "END")),
    ])
});

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let mut buffer = String::new();

    loop {
        if let Err(e) = std::io::stdin().read_line(&mut buffer) {
            eprintln!("{}", e);
            break;
        }

        if buffer.is_empty() {
            continue;
        }

        let text = buffer.trim_end().to_string().chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&text);
        let parser = Parser::new(lexer)?;
        let mut interpreter = Interpreter::new(parser);
        let result = interpreter.interpret()?;

        println!("{:?}", result);
        buffer.clear();
    }

    Ok(())
}

trait Visitor {
    fn visit(&self, node: &AstNode) -> isize {
        match node {
            AstNode::UnaryOp(unary) => self.visit_unaryop(unary),
            AstNode::BinOps(bin_op) => self.visit_binop(bin_op),
            AstNode::Number(num) => self.visit_num(num),
        }
    }

    fn visit_num(&self, node: &Num) -> isize;
    fn visit_unaryop(&self, node: &Unary) -> isize;
    fn visit_binop(&self, node: &BinOp) -> isize;
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
    Integer,
    Plus,
    Minus,
    Mul,
    Div,
    ID,
    Assign,
    Semi,
    Dot,
    Begin,
    End,
    LParen,
    RParen,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    kind: TokenType,
    value: String,
}

impl AsRef<Token> for Token {
    fn as_ref(&self) -> &Token {
        self
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = &self.kind;
        let value = &self.value;

        write!(f, "Token({:?}, {})", kind, value)
    }
}

impl Token {
    fn new(kind: TokenType, value: impl ToString) -> Self {
        Self {
            kind,
            value: value.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
enum AstNode {
    /// Node which can represent all four binary operators.
    BinOps(BinOp),
    UnaryOp(Unary),
    Number(Num),
}

#[derive(Debug, Clone)]
struct BinOp {
    lhs: Box<AstNode>,
    token: Token,
    rhs: Box<AstNode>,
}

#[derive(Debug, Clone)]
struct Unary {
    token: Token,
    expr: Box<AstNode>,
}

#[derive(Debug, Clone)]
struct Num(isize);

impl std::ops::Deref for Num {
    type Target = isize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct Lexer<'a> {
    text: &'a [char],
    pos: usize,
    current_char: char,
}

impl<'a> Lexer<'a> {
    fn new(text: &'a [char]) -> Self {
        Self {
            text,
            pos: 0,
            current_char: text[0],
        }
    }

    /// Advance position in text and set the current char.
    fn advance(&mut self) {
        self.pos += 1;

        match self.pos > self.text.len() - 1 {
            true => self.current_char = '\0',
            false => self.current_char = self.text[self.pos],
        }
    }

    /// Advance until the current char is neither a null char nor whitespace.
    fn skip_whitespace(&mut self) {
        while self.current_char != '\0' && self.current_char.is_whitespace() {
            self.advance();
        }
    }

    /// Return a integer consumed from the input
    fn integer(&mut self) -> String {
        let mut result = String::new();
        while self.current_char != '\0' && self.current_char.is_ascii_digit() {
            result.push(self.current_char);
            self.advance();
        }

        result
    }

    /// Handle identifiers and reserved keywords
    fn id(&mut self) -> Token {
        let mut result = String::new();
        while self.current_char != '\0' && self.current_char.is_alphanumeric() {
            result.push(self.current_char);
            self.advance();
        }

        RESERVED_KEYWORDS
            .get(result.as_str())
            .unwrap_or(&Token::new(TokenType::ID, result.clone()))
            .to_owned()
    }

    /// Get the next `char` without consuming it.
    fn peek(&self) -> Option<char> {
        let peek_pos = self.pos + 1;
        match peek_pos > self.text.len() - 1 {
            true => None,
            false => Some(self.text[peek_pos]),
        }
    }

    /// The method is responsible for breaking sentences apart and return one token at a time.
    ///
    /// # Errors
    ///
    /// This method errors if the character does not match any of the defined token types.
    fn get_next_token(&mut self) -> Result<Token> {
        while self.current_char != '\0' {
            match self.current_char {
                c if c.is_whitespace() => {
                    self.skip_whitespace();
                    continue;
                }
                c if c.is_ascii_digit() => {
                    return Ok(Token::new(TokenType::Integer, self.integer()));
                }
                c if c.is_alphanumeric() => return Ok(self.id()),
                c if c == ':' && self.peek() == Some('=') => {
                    self.advance();
                    self.advance();

                    return Ok(Token::new(TokenType::Assign, ":="));
                }
                ';' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Semi, ';'));
                }
                '.' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Dot, '.'));
                }
                '+' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Plus, '+'));
                }
                '-' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Minus, '-'));
                }
                '*' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Mul, '*'));
                }
                '/' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Div, '/'));
                }
                '(' => {
                    self.advance();
                    return Ok(Token::new(TokenType::LParen, '('));
                }
                ')' => {
                    self.advance();
                    return Ok(Token::new(TokenType::RParen, ')'));
                }
                c => {
                    // TODO: improve error types
                    return Err(format!("Error parsing input: {}", c).into());
                }
            }
        }

        Ok(Token::new(TokenType::Eof, '\0'))
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Result<Self> {
        let current_token = lexer.get_next_token()?;

        Ok(Self {
            lexer,
            current_token,
        })
    }

    /// Compare current token type to with the passed tokens type, eat the current token if they
    /// match and assign the next token as the `current_token`.
    ///
    /// # Errors
    ///
    /// If the tokens do not match.
    fn eat(&mut self, token_type: TokenType) -> Result<()> {
        if self.current_token.kind == token_type {
            self.current_token = self.lexer.get_next_token()?;

            return Ok(());
        }

        Err("Invalid Syntax".into())
    }

    /// `factor : (PLUS | MINUS ) factor | INTEGER | LPAREN expr RPAREN`
    ///
    /// Return an `Integer` token value.
    ///
    /// # Errors
    ///
    /// This method errors if the current token does not match with the eaten token.
    fn factor(&mut self) -> Result<AstNode> {
        let token = self.current_token.clone();
        match token.kind {
            TokenType::Plus => {
                self.eat(TokenType::Plus)?;

                Ok(AstNode::UnaryOp(Unary {
                    token,
                    expr: Box::new(self.factor()?),
                }))
            }
            TokenType::Minus => {
                self.eat(TokenType::Minus)?;

                Ok(AstNode::UnaryOp(Unary {
                    token,
                    expr: Box::new(self.factor()?),
                }))
            }
            TokenType::Integer => {
                self.eat(TokenType::Integer)?;

                Ok(AstNode::Number(Num(token.value.parse()?)))
            }
            TokenType::LParen => {
                self.eat(TokenType::LParen)?;
                let node = self.expr()?;
                self.eat(TokenType::RParen)?;

                Ok(node)
            }
            _ => todo!(),
        }
    }

    /// `term : factor ((MUL | DIV) factor)*`
    ///
    /// Return result of multiplication or division op.
    ///
    /// # Errors
    ///
    /// This method errors if the arithmetic operation causes the `result` to overflow/underflow.
    fn term(&mut self) -> Result<AstNode> {
        let mut node = self.factor()?;

        while let TokenType::Mul | TokenType::Div = self.current_token.kind {
            let token = self.current_token.clone();
            match token.kind {
                TokenType::Mul => {
                    self.eat(TokenType::Mul)?;
                }
                TokenType::Div => {
                    self.eat(TokenType::Div)?;
                }
                _ => unreachable!(),
            }

            node = AstNode::BinOps(BinOp {
                lhs: Box::new(node),
                token: token.clone(),
                rhs: Box::new(self.factor()?),
            })
        }

        Ok(node)
    }

    /// Arithmetic expression parser / interpreter.
    ///
    /// # Supported grammar
    ///
    /// `expr : term ((PLUS | MINUS) term)*`
    /// `term : factor ((MUL | DIV) factor)*`
    /// `factor : (PLUS | MINUS ) factor | INTEGER | LPAREN expr RPAREN`
    ///
    /// # Errors
    ///
    /// This method errors if the input cannot be tokenized, if we could not "parse" the token
    /// values or if the arithmetic operation on the result causes it to overflow/underflow.
    fn expr(&mut self) -> Result<AstNode> {
        let mut node = self.term()?;

        while let TokenType::Plus | TokenType::Minus = self.current_token.kind {
            let token = self.current_token.clone();
            match token.kind {
                TokenType::Plus => {
                    self.eat(TokenType::Plus)?;
                }
                TokenType::Minus => {
                    self.eat(TokenType::Minus)?;
                }
                _ => unreachable!(),
            }

            node = AstNode::BinOps(BinOp {
                lhs: Box::new(node),
                token: token.clone(),
                rhs: Box::new(self.factor()?),
            })
        }

        Ok(node)
    }

    fn parse(&mut self) -> Result<AstNode> {
        self.expr()
    }
}

struct Interpreter<'a> {
    parser: Parser<'a>,
}

impl<'a> Visitor for Interpreter<'a> {
    fn visit_num(&self, node: &Num) -> isize {
        **node // this feels pretty dumb...
    }

    fn visit_unaryop(&self, node: &Unary) -> isize {
        match node.token.kind {
            TokenType::Plus => self.visit(&node.expr),
            TokenType::Minus => -self.visit(&node.expr),
            _ => unreachable!(),
        }
    }

    fn visit_binop(&self, node: &BinOp) -> isize {
        let lhs = self.visit(&node.lhs);
        let rhs = self.visit(&node.rhs);

        match node.token.kind {
            TokenType::Plus => lhs + rhs,
            TokenType::Minus => lhs - rhs,
            TokenType::Mul => lhs * rhs,
            TokenType::Div => lhs / rhs,
            _ => unreachable!(),
        }
    }
}

impl<'a> Interpreter<'a> {
    fn new(parser: Parser<'a>) -> Self {
        Self { parser }
    }

    fn interpret(&mut self) -> Result<isize> {
        let tree = self.parser.parse()?;
        Ok(self.visit(&tree))
    }
}

#[cfg(test)]
mod tests {
    use crate::{Interpreter, Lexer, Parser, Result, Token, TokenType};

    fn interpret(input: String) -> Result<isize> {
        let text = input.trim_end().to_string().chars().collect::<Vec<_>>();
        let lexer = Lexer::new(&text);
        let parser = Parser::new(lexer)?;
        let mut interpreter = Interpreter::new(parser);

        interpreter.interpret()
    }

    #[test]
    fn unary_ops() {
        let input = String::from("- 3");
        assert_eq!(interpret(input).unwrap(), -3);

        let input = String::from("+ 3");
        assert_eq!(interpret(input).unwrap(), 3);

        let input = String::from("5 - - - + - 3");
        assert_eq!(interpret(input).unwrap(), 8);

        let input = String::from("5 - - - + - (3 + 4) - +2");
        assert_eq!(interpret(input).unwrap(), 10);
    }

    #[test]
    fn returns_program_tokens() {
        let input = String::from("BEGIN a := 2; END.")
            .chars()
            .collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);

        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::Begin, "BEGIN")
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::ID, "a")
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::Assign, ":=")
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::Integer, "2")
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::Semi, ";")
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::End, "END")
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::Dot, '.')
        );
        assert_eq!(
            lexer.get_next_token().unwrap(),
            Token::new(TokenType::Eof, '\0')
        );
    }
}
