fn main() -> Result<(), Box<dyn std::error::Error>> {
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
        let mut interpreter = Parser::new(lexer)?;
        let result = interpreter.expr()?;

        println!("{:?}", result);
        buffer.clear();
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
    Integer,
    Plus,
    Minus,
    Mul,
    Div,
    LParen,
    RParen,
    Eof,
}

#[derive(Debug, Clone)]
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

#[derive(Debug)]
enum AstNode {
    /// Node which can represent all four binary operators.
    BinOp {
        lhs: Box<AstNode>,
        token: Token,
        op: String,
        rhs: Box<AstNode>,
    },
    Number {
        token: Token,
        value: String,
    },
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

    /// The method is responsible for breaking sentences apart and return one token at a time.
    ///
    /// # Errors
    ///
    /// This method errors if the character does not match any of the defined token types.
    fn get_next_token(&mut self) -> Result<Token, Box<dyn std::error::Error>> {
        while self.current_char != '\0' {
            match self.current_char {
                c if c.is_whitespace() => {
                    self.skip_whitespace();
                    continue;
                }
                c if c.is_ascii_digit() => {
                    return Ok(Token::new(TokenType::Integer, self.integer()));
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
    fn new(mut lexer: Lexer<'a>) -> Result<Self, Box<dyn std::error::Error>> {
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
    fn eat(&mut self, token_type: TokenType) -> Result<(), Box<dyn std::error::Error>> {
        if self.current_token.kind == token_type {
            self.current_token = self.lexer.get_next_token()?;

            return Ok(());
        }

        Err("Invalid Syntax".into())
    }

    /// `factor : INTEGER | LPAREN expr RPAREN`
    ///
    /// Return an `Integer` token value.
    ///
    /// # Errors
    ///
    /// This method errors if the current token does not match with the eaten token or if the
    /// token value cannot be parsed to a `u32`.
    fn factor(&mut self) -> Result<AstNode, Box<dyn std::error::Error>> {
        let token = self.current_token.clone();
        if token.kind == TokenType::Integer {
            self.eat(TokenType::Integer)?;
            return Ok(AstNode::Number {
                token: token.clone(),
                value: token.value,
            });
        }

        // We're expecting the token to be a `LPAREN`
        self.eat(TokenType::LParen)?;
        let node = self.expr()?;
        self.eat(TokenType::RParen)?;

        Ok(node)
    }

    /// `term : factor ((MUL | DIV) factor)*`
    ///
    /// Return result of multiplication or division op.
    ///
    /// # Errors
    ///
    /// This method errors if the arithmetic operation causes the `result` to overflow/underflow.
    fn term(&mut self) -> Result<AstNode, Box<dyn std::error::Error>> {
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

            node = AstNode::BinOp {
                lhs: Box::new(node),
                token: token.clone(),
                op: token.value,
                rhs: Box::new(self.factor()?),
            }
        }

        Ok(node)
    }

    /// Arithmetic expression parser / interpreter.
    ///
    /// # Supported grammar
    ///
    /// `expr : term ((PLUS | MINUS) term)*`
    /// `term : factor ((MUL | DIV) factor)*`
    /// `factor : INTEGER | LPAREN expr RPAREN`
    ///
    /// # Errors
    ///
    /// This method errors if the input cannot be tokenized, if we could not "parse" the token
    /// values or if the arithmetic operation on the result causes it to overflow/underflow.
    fn expr(&mut self) -> Result<AstNode, Box<dyn std::error::Error>> {
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

            node = AstNode::BinOp {
                lhs: Box::new(node),
                token: token.clone(),
                op: token.value,
                rhs: Box::new(self.term()?),
            }
        }

        Ok(node)
    }
}
