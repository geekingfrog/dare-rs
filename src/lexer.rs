use std::{fmt::Display, iter::Peekable, str::Chars};

// used in tests to enumerate all variants of enums
use strum::IntoEnumIterator;

#[derive(Debug, PartialEq, Eq)]
pub enum Tok {
    OpenParens,
    CloseParens,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    LesserThan,
    GreaterThan,
    Comma,
    QuestionMark,
    Newline,
    // Skipped(char),
    Keyword(Keyword),
    Str(String),
    CapitalizedLabel(String),
    Label(String),
}

fn lookup_atomic_token(c: &char) -> Option<Tok> {
    match c {
        '(' => Some(Tok::OpenParens),
        ')' => Some(Tok::CloseParens),
        '{' => Some(Tok::OpenBrace),
        '}' => Some(Tok::CloseBrace),
        '[' => Some(Tok::OpenBracket),
        ']' => Some(Tok::CloseBracket),
        '<' => Some(Tok::LesserThan),
        '>' => Some(Tok::GreaterThan),
        ',' => Some(Tok::Comma),
        '?' => Some(Tok::QuestionMark),
        '\n' => Some(Tok::Newline),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq, EnumIter)]
pub enum Keyword {
    Enum,
    Struct,
    Type,
    List,
    Map,
    Optional,
    Str,
    UInt,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float,
    Bool,
    Bytes,
    As,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Enum => f.write_str("enum"),
            Keyword::Struct => f.write_str("struct"),
            Keyword::Type => f.write_str("type"),
            Keyword::List => f.write_str("List"),
            Keyword::Map => f.write_str("Map"),
            Keyword::Optional => f.write_str("Optional"),
            Keyword::Str => f.write_str("String"),
            Keyword::UInt => f.write_str("UInt"),
            Keyword::Int => f.write_str("Int"),
            Keyword::Int8 => f.write_str("Int8"),
            Keyword::Int16 => f.write_str("Int16"),
            Keyword::Int32 => f.write_str("Int32"),
            Keyword::Int64 => f.write_str("Int64"),
            Keyword::UInt8 => f.write_str("UInt8"),
            Keyword::UInt16 => f.write_str("UInt16"),
            Keyword::UInt32 => f.write_str("UInt32"),
            Keyword::UInt64 => f.write_str("UInt64"),
            Keyword::Float => f.write_str("Float"),
            Keyword::Bool => f.write_str("Bool"),
            Keyword::Bytes => f.write_str("Bytes"),
            Keyword::As => f.write_str("As"),
        }
    }
}

fn lookup_keyword(raw: &str) -> Option<Keyword> {
    match raw {
        "enum" => Some(Keyword::Enum),
        "struct" => Some(Keyword::Struct),
        "type" => Some(Keyword::Type),
        "List" => Some(Keyword::List),
        "Map" => Some(Keyword::Map),
        "Optional" => Some(Keyword::Optional),
        "String" => Some(Keyword::Str),
        "UInt" => Some(Keyword::UInt),
        "Int" => Some(Keyword::Int),
        "Int8" => Some(Keyword::Int8),
        "Int16" => Some(Keyword::Int16),
        "Int32" => Some(Keyword::Int32),
        "Int64" => Some(Keyword::Int64),
        "UInt8" => Some(Keyword::UInt8),
        "UInt16" => Some(Keyword::UInt16),
        "UInt32" => Some(Keyword::UInt32),
        "UInt64" => Some(Keyword::UInt64),
        "Float" => Some(Keyword::Float),
        "Bool" => Some(Keyword::Bool),
        "Bytes" => Some(Keyword::Bytes),
        "As" => Some(Keyword::As),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexicalError {
    UnexpectedEOF,
    UnterminatedString(Loc, Loc),
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    line: usize,
    column: usize,
}

impl Loc {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    fn next_char(&mut self) -> &Self {
        self.column = self.column + 1;
        self
    }

    fn next_line(&mut self) -> &Self {
        self.column = 1;
        self.line = self.line + 1;
        self
    }
}

pub struct Lexer<'input> {
    chars: Peekable<Chars<'input>>,
    current_loc: Loc,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.chars().peekable(),
            current_loc: Loc::new(1, 1),
        }
    }

    pub fn run(&mut self) -> Vec<Spanned<Tok, Loc, LexicalError>> {
        self.into_iter().collect()
    }

    // pub fn scan(&mut self) -> Box<dyn Iterator<Item=Spanned<Tok, Loc, LexicalError>>> {
    //     Box::new(self.into_iter())
    // }

    fn do_stuff(&mut self) -> Option<usize> {
        todo!()
    }

    fn parse_word(&mut self) -> Result<String, LexicalError> {
        todo!()
    }

    /// consume one character from the input stream, returns it
    /// and adjust the `current_loc` field
    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.current_loc.next_line();
                } else {
                    self.current_loc.next_char();
                }
                Some(c)
            }
            None => None,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, Loc, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut start_loc = self.current_loc;
        loop {
            match self.advance() {
                Some(c) => {
                    if let Some(tok) = lookup_atomic_token(&c) {
                        return Some(Ok((start_loc, tok, self.current_loc)));
                    }
                    if c == '"' {
                        let mut chars: Vec<char> = Vec::new();
                        loop {
                            match self.advance() {
                                Some(c) if c == '"' => {
                                    return Some(Ok((
                                        start_loc,
                                        Tok::Str(chars.into_iter().collect()),
                                        self.current_loc,
                                    )));
                                }
                                // Strings cannot spans multiple lines
                                Some(c) if c == '\n' => {
                                    return Some(Err(LexicalError::UnterminatedString(
                                        start_loc,
                                        self.current_loc,
                                    )))
                                }
                                Some(c) => chars.push(c),
                                None => {
                                    return Some(Err(LexicalError::UnterminatedString(
                                        start_loc,
                                        self.current_loc,
                                    )))
                                }
                            }
                        }
                    }

                    // output a label
                    if c.is_ascii_alphabetic() {
                        let is_uppercase_word = c.is_uppercase();
                        let mut chars: Vec<char> = vec![c];
                        loop {
                            match self.advance() {
                                Some(c) if c.is_ascii_alphanumeric() => chars.push(c),
                                Some(c) if c == ' ' => {
                                    let label: String = chars.into_iter().collect();
                                    let token = match lookup_keyword(&label) {
                                        Some(kw) => Tok::Keyword(kw),
                                        None => {
                                            if is_uppercase_word {
                                                Tok::CapitalizedLabel(label)
                                            } else {
                                                Tok::Label(label)
                                            }
                                        }
                                    };
                                    return Some(Ok((start_loc, token, self.current_loc)));
                                }
                                None => {
                                    // copy pasted from the previous block
                                    let label: String = chars.into_iter().collect();
                                    let token = match lookup_keyword(&label) {
                                        Some(kw) => Tok::Keyword(kw),
                                        None => {
                                            if is_uppercase_word {
                                                Tok::CapitalizedLabel(label)
                                            } else {
                                                Tok::Label(label)
                                            }
                                        }
                                    };
                                    return Some(Ok((start_loc, token, self.current_loc)));
                                }
                                _ => {
                                    return Some(Err(LexicalError::UnterminatedString(
                                        start_loc,
                                        self.current_loc,
                                    )))
                                }
                            }
                        }
                    }
                    // skip the token
                    start_loc = self.current_loc;
                    continue;
                    // return Some(Ok((start_loc, Tok::Skipped(c), self.current_loc)));
                }
                None => return None, // EOF
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_lexer() {
        // let tokens: Vec<Spanned<Tok, Loc, LexicalError>> = Lexer::new("enum").into_iter().collect();
        let tokens = Lexer::new("enum").run();
        let expected = vec![Ok((
            Loc { line: 1, column: 1 },
            Tok::Keyword(Keyword::Enum),
            Loc { line: 1, column: 5 },
        ))];
        assert!('e'.is_ascii_alphabetic());
        assert_eq!(tokens, expected, "can parse keyword");
    }

    #[test]
    fn test_keywords() {
        for variant in Keyword::iter() {
            let name = format!("{}", variant);
            let tokens = Lexer::new(&name).run();
            let expected = vec![Ok((
                Loc { line: 1, column: 1 },
                Tok::Keyword(variant),
                Loc {
                    line: 1,
                    column: name.len() + 1,
                },
            ))];
            assert_eq!(tokens, expected, "can parse keyword: {}", name);
        }
    }

    #[test]
    fn test_roundtrip_keyword() {
        for variant in Keyword::iter() {
            let name = format!("{}", variant);
            let tokens = Lexer::new(&name).run();
            let roundtripped = tokens
                .into_iter()
                .map(|t| match t {
                    Ok((_, tok, _)) => match tok {
                        Tok::Keyword(kw) => Ok(format!("{}", kw)),
                        _ => Err("Expected keyword but got something else".to_string()),
                    },
                    Err(err) => Err(format!("{:?}", err)),
                })
                .collect::<Vec<_>>();

            let expected = vec![Ok(name.clone())];
            assert_eq!(roundtripped, expected, "roundtrip for keyword {}", name);
        }
    }

    #[test]
    fn test_struct() {
        let tokens = Lexer::new("enum  {}")
            .run()
            .into_iter()
            .map(|r| r.map(|(_, tok, _)| tok))
            .collect::<Vec<_>>();
        let expected = vec![
            Ok(Tok::Keyword(Keyword::Enum)),
            Ok(Tok::OpenBrace),
            Ok(Tok::CloseBrace),
        ];
        assert_eq!(tokens, expected);
    }
}
