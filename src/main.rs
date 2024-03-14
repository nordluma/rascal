fn main() {
    println!("Hello, world!");
}

#[derive(Debug)]
enum TokenType {
    Integer,
    Plus,
    Eof,
}

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

struct Token {
    kind: TokenType,
    value: TokenValue,
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
