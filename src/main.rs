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

        let chars = buffer.trim_end().to_string().chars().collect::<Vec<_>>();
        let mut interpreter = Interpreter::new(&chars);
        let result = interpreter.expr()?;

        println!("{}", result);
        buffer.clear();
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
    Integer,
    Plus,
    Minus,
    Eof,
    None,
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
    fn new(kind: TokenType, value: String) -> Self {
        Self { kind, value }
    }
}

struct Interpreter<'a> {
    text: &'a [char],
    pos: usize,
    current_token: Token,
    current_char: char,
}

impl<'a> Interpreter<'a> {
    fn new(text: &'a [char]) -> Self {
        Self {
            text,
            pos: 0,
            current_token: Token::new(TokenType::None, '\0'.to_string()),
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

        // we have previously checked that the chars are ascii digits so it
        // should be safe to unwrap
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
                    return Ok(Token::new(TokenType::Plus, '+'.to_string()));
                }
                '-' => {
                    self.advance();
                    return Ok(Token::new(TokenType::Minus, '-'.to_string()));
                }
                c => {
                    // TODO: improve error types
                    return Err(format!("Error parsing input: {}", c).into());
                }
            }
        }

        Ok(Token::new(TokenType::Eof, '\0'.to_string()))
    }

    /// Compare current token type to with the passed tokens type, eat the current token if they
    /// match and assign the next token as the `current_token`.
    ///
    /// # Errors
    ///
    /// If the tokens do not match.
    fn eat(&mut self, token_type: TokenType) -> Result<(), Box<dyn std::error::Error>> {
        if self.current_token.kind == token_type {
            self.current_token = self.get_next_token()?;

            return Ok(());
        }

        Err("tokens do not match".into())
    }

    /// Return an Integer token value.
    ///
    /// # Errors
    ///
    /// This method errors if the current token does not match with the eaten token or if the
    /// token value cannot be parsed to a `u32`.
    fn term(&mut self) -> Result<u32, Box<dyn std::error::Error>> {
        let token = self.current_token.clone();
        self.eat(TokenType::Integer)?;

        token
            .value
            .parse()
            .map_err(|e| format!("could not parse: {}", e).into())
    }

    /// Evaluate the expression.
    ///
    /// # Errors
    ///
    /// This method errors if the input cannot be tokenized, if we could not "parse" the token
    /// values or if the arithmetic operation on the result causes it to overflow/underflow.
    fn expr(&mut self) -> Result<u32, Box<dyn std::error::Error>> {
        // set current token to be the first token taken from the the input
        self.current_token = self.get_next_token()?;

        let mut result = self.term()?;
        while let TokenType::Plus | TokenType::Minus = self.current_token.kind {
            let token = self.current_token.as_ref();
            match token.kind {
                TokenType::Plus => {
                    self.eat(TokenType::Plus)?;
                    result = result + self.term()?;
                }
                TokenType::Minus => {
                    self.eat(TokenType::Minus)?;
                    result = result - self.term()?;
                }
                _ => {}
            }
        }

        Ok(result)
    }
}
