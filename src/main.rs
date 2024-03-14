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
    Eof,
    None,
}

#[derive(Debug, Clone)]
enum TokenValue {
    Integer(u32),
    Plus(char),
    None,
}

impl TryInto<u32> for TokenValue {
    type Error = Box<dyn std::error::Error>;

    fn try_into(self) -> Result<u32, Self::Error> {
        if let TokenValue::Integer(num) = self {
            Ok(num)
        } else {
            Err("Not a number".into())
        }
    }
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
    current_token: Token,
}

impl<'a> Interpreter<'a> {
    fn new(text: &'a [char]) -> Self {
        Self {
            text,
            pos: 0,
            current_token: Token::new(TokenType::None, TokenValue::None),
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

        Err(format!("Error parsing input: {}", current_char).into()) // TODO: improve error types
    }

    /// Compare current token type to with the passed tokens type, eat the current token if they
    /// match and assign the next token as the `current_token`.
    ///
    /// # Errors
    ///
    /// If the tokens do not match.
    fn eat(&mut self, token_type: TokenType) -> Result<(), Box<dyn std::error::Error>> {
        let matches = self.current_token.as_ref().kind == token_type;

        match matches {
            true => {
                self.current_token = self.get_next_token()?;
                Ok(())
            }
            false => Err("tokens do not match".into()),
        }
    }

    /// Evaluate the expression.
    ///
    /// # Errors
    ///
    /// This method errors if the input cannot be tokenized or if we could not "parse" the token
    /// values.
    fn expr(&mut self) -> Result<u32, Box<dyn std::error::Error>> {
        // set current token to be the first token taken from the the input
        self.current_token = self.get_next_token()?;

        // the next token should be a single-digit integer
        let left: u32 = self.current_token.as_ref().value.clone().try_into()?;
        self.eat(TokenType::Integer)?;

        // the next token should be a '+' token
        let _op = self.current_token.as_ref();
        self.eat(TokenType::Plus)?;

        let right: u32 = self.current_token.as_ref().value.clone().try_into()?;
        self.eat(TokenType::Integer)?;

        Ok(left + right)
    }
}
