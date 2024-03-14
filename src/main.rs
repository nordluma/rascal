fn main() {
    println!("Hello, world!");
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenType {
    Integer,
    Plus,
    Eof,
}

#[derive(Debug, Clone)]
enum TokenValue {
    Integer(u32),
    Plus(char),
    None,
}

impl std::fmt::Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::Integer(int) => write!(f, "{}", int),
            TokenValue::Plus(c) => write!(f, "{}", c),
            TokenValue::None => write!(f, "None"),
        }
    }
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenType,
    value: TokenValue,
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
    fn new(kind: TokenType, value: TokenValue) -> Self {
        Self { kind, value }
    }
}

struct Interpreter<'a> {
    text: &'a [char],
    pos: usize,
    current_token: Option<Token>,
}

impl<'a> Interpreter<'a> {
    fn new(text: &'a [char]) -> Self {
        Self {
            text,
            pos: 0,
            current_token: None,
        }
    }

    /// The method is responsible for breaking sentences apart and return one token at a time.
    ///
    /// # Errors
    ///
    /// This method errors if the character does not match any of the defined token types.
    fn get_next_token(&mut self) -> Result<Token, Box<dyn std::error::Error>> {
        let text = self.text;

        // if the index is past the end of the text we'll return `Eof` since
        // there is nothing left to tokenize
        if self.pos > text.len() - 1 {
            return Ok(Token::new(TokenType::Eof, TokenValue::None));
        }

        // get the character at the current position and create a token based
        // on the type of character
        let current_char = text[self.pos];

        if current_char.is_ascii_digit() {
            let token = Token::new(
                TokenType::Integer,
                TokenValue::Integer(current_char.to_digit(10).expect("Char should be a digit")),
            );

            self.pos += 1;

            return Ok(token);
        }

        if current_char == '+' {
            let token = Token::new(TokenType::Plus, TokenValue::Plus(current_char));
            self.pos += 1;

            return Ok(token);
        }

        Err("Error parsing input".into()) // TODO: improve error types
    }

    /// Compare current token type to with the passed tokens type, eat the current token if they
    /// match and assign the next token as the `current_token`.
    ///
    /// # Errors
    ///
    /// If the tokens do not match.
    fn eat(&mut self, token_type: TokenType) -> Result<(), Box<dyn std::error::Error>> {
        let matches = self
            .current_token
            .as_ref()
            .is_some_and(|t| t.kind == token_type);

        match matches {
            true => {
                self.current_token = Some(self.get_next_token()?);
                Ok(())
            }
            false => Err("tokens do not match".into()),
        }
    }
}
