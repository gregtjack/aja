use crate::token::Token;
use ecow::EcoString;
use std::{fmt, iter::Peekable};

use crate::{
    ast::{Location, Source},
    diagnostic::Diagnostic,
};

#[derive(Debug)]
pub struct LexicalError {
    pub start: Location,
    pub end: Location,
    pub message: String,
    pub help: Option<String>,
}

impl std::error::Error for LexicalError {}

impl LexicalError {
    fn new(start: Location, end: Location, msg: impl Into<String>) -> Self {
        LexicalError {
            start,
            end,
            message: msg.into(),
            help: None,
        }
    }

    fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::new(self.message.clone())
            .with_span(self.start, self.end)
            .with_help(self.help.clone())
    }
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}: {}",
            self.start.line, self.start.col, self.message
        )
    }
}

pub type SrcSpan = (Location, Token, Location);
pub type LexResult = Result<SrcSpan, LexicalError>;

pub struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    inner: Peekable<T>,
    position: u32,
    line: u32,
    col: u32,
    eof: bool,
    source: Source,
    /// A token already read, to give before reading more: the `..` or `.`
    /// that ends a number, as in `0..10` or `3.min(x)`.
    pending: Option<SrcSpan>,
    /// For each template string being lexed, innermost last, how many `{`
    /// of its current expression are still open: a `}` when it is 0 ends
    /// the expression and goes back to the string's text.
    templates: Vec<u32>,
    /// Whether the last token was a `.`, so a number is a tuple field's
    /// position and has no fraction: `pair.0.1` is `pair.0`, then `.1`.
    after_dot: bool,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(input: T) -> Self {
        Lexer {
            inner: input.peekable(),
            position: 0,
            line: 1,
            col: 1,
            eof: false,
            source: Source::MAIN,
            pending: None,
            templates: Vec::new(),
            after_dot: false,
        }
    }

    /// Process one token.
    pub fn token(&mut self) -> LexResult {
        let result = self.next_token();
        self.after_dot = matches!(result, Ok((_, Token::Dot, _)));
        result
    }

    fn next_token(&mut self) -> LexResult {
        if let Some(t) = self.pending.take() {
            return Ok(t);
        }
        let mut start = self.get_location();
        while let Some(c) = self.inner_next() {
            match c {
                '0'..='9' => {
                    return self.tok_number(c, start);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    return self.tok_keyword_or_ident(c, start);
                }
                // ignore whitespace
                ' ' | '\t' | '\r' => {}
                '\n' => self.advance_line(),
                '(' => return Ok((start, Token::LeftParen, self.get_location())),
                ')' => return Ok((start, Token::RightParen, self.get_location())),
                '{' => {
                    if let Some(depth) = self.templates.last_mut() {
                        *depth += 1;
                    }
                    return Ok((start, Token::LeftBrace, self.get_location()));
                }
                '}' => match self.templates.last_mut() {
                    Some(0) => {
                        self.templates.pop();
                        return self.tok_str(start, true);
                    }
                    Some(depth) => {
                        *depth -= 1;
                        return Ok((start, Token::RightBrace, self.get_location()));
                    }
                    None => return Ok((start, Token::RightBrace, self.get_location())),
                },
                '[' => return Ok((start, Token::LeftBracket, self.get_location())),
                ']' => return Ok((start, Token::RightBracket, self.get_location())),
                '|' => {
                    if self.match_advance('|') {
                        return Ok((start, Token::Or, self.get_location()));
                    } else {
                        return Ok((start, Token::Pipe, self.get_location()));
                    }
                }
                ',' => return Ok((start, Token::Comma, self.get_location())),
                '.' => {
                    if self.match_advance('.') {
                        return Ok((start, Token::DotDot, self.get_location()));
                    }
                    return Ok((start, Token::Dot, self.get_location()));
                }
                ':' => {
                    if self.match_advance(':') {
                        return Ok((start, Token::DoubleColon, self.get_location()));
                    } else {
                        return Ok((start, Token::Colon, self.get_location()));
                    }
                }
                ';' => return Ok((start, Token::Semicolon, self.get_location())),
                '"' => return self.tok_str(start, false),
                '+' => return Ok((start, Token::Plus, self.get_location())),
                '-' => {
                    if self.match_advance('>') {
                        return Ok((start, Token::RightArrow, self.get_location()));
                    } else {
                        return Ok((start, Token::Minus, self.get_location()));
                    }
                }
                '*' => return Ok((start, Token::Mult, self.get_location())),
                '!' | '=' | '>' | '<' | '&' => {
                    let op = self.tok_op(c, start)?;
                    return Ok(op);
                }
                '/' => {
                    if self.match_advance('/') {
                        self.tok_comment();
                    } else {
                        return Ok((start, Token::Div, self.get_location()));
                    }
                }
                '%' => return Ok((start, Token::Modulo, self.get_location())),
                '?' => return Ok((start, Token::Question, self.get_location())),
                _ => {
                    return Err(LexicalError::new(
                        start,
                        self.get_location(),
                        format!("unexpected character '{}'", c),
                    ))
                }
            }
            // Only whitespace and comments reach here: the next token starts after them.
            start = self.get_location();
        }

        if !self.templates.is_empty() {
            self.templates.clear();
            return Err(LexicalError::new(
                start,
                self.get_location(),
                "unterminated expression in a template string",
            )
            .help("close the expression with '}', or write '\\{' for a literal brace"));
        }
        self.eof = true;
        Ok((start, Token::Eof, self.get_location()))
    }

    /// Advance the iterator
    fn inner_next(&mut self) -> Option<char> {
        self.col += 1;
        self.position += 1;
        self.inner.next()
    }

    /// Peek the next character in the iterator
    fn inner_peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    /// Increment the line counter and reset the current column
    fn advance_line(&mut self) {
        self.col = 1;
        self.line += 1;
    }

    /// Advance self if the current char is a match
    fn match_advance(&mut self, expect: char) -> bool {
        match self.inner_peek() {
            None => false,
            Some(&c) => {
                if c == expect {
                    self.inner_next();
                    true
                } else {
                    false
                }
            }
        }
    }

    fn get_location(&self) -> Location {
        Location {
            line: self.line,
            col: self.col,
            pos: self.position,
            source: self.source,
        }
    }

    /// Tokenize double slash (//) comments
    fn tok_comment(&mut self) {
        while let Some(&c) = self.inner_peek() {
            if c == '\n' {
                // Leave the newline for `token` so line counting stays in one place.
                return;
            }
            self.inner_next();
        }
    }

    /// Tokenize
    fn tok_op(&mut self, c: char, start: Location) -> LexResult {
        match c {
            '!' => {
                if self.match_advance('=') {
                    Ok((start, Token::BangEqual, self.get_location()))
                } else {
                    Ok((start, Token::Bang, self.get_location()))
                }
            }
            '=' => {
                if self.match_advance('=') {
                    Ok((start, Token::EqualEqual, self.get_location()))
                } else if self.match_advance('>') {
                    Ok((start, Token::FatArrow, self.get_location()))
                } else {
                    Ok((start, Token::Equal, self.get_location()))
                }
            }
            '>' => {
                if self.match_advance('=') {
                    Ok((start, Token::GreaterEqual, self.get_location()))
                } else {
                    Ok((start, Token::GreaterThan, self.get_location()))
                }
            }
            '<' => {
                if self.match_advance('=') {
                    Ok((start, Token::LessEqual, self.get_location()))
                } else if self.match_advance('-') {
                    Ok((start, Token::LeftArrow, self.get_location()))
                } else {
                    Ok((start, Token::LessThan, self.get_location()))
                }
            }
            '&' => {
                if self.match_advance('&') {
                    Ok((start, Token::And, self.get_location()))
                } else {
                    Err(
                        LexicalError::new(start, self.get_location(), "unexpected character '&'")
                            .help("use '&&' for logical and"),
                    )
                }
            }
            _ => Err(LexicalError::new(
                start,
                self.get_location(),
                "invalid operator",
            )),
        }
    }

    /// A string's text, from after its opening `"`, or (if `resumed`) after
    /// the `}` that ends one of its expressions, up to the closing `"` or
    /// the `{` that starts its next expression.
    fn tok_str(&mut self, start: Location, resumed: bool) -> LexResult {
        let mut literal = EcoString::new();
        while let Some(&c) = self.inner_peek() {
            match c {
                '"' => {
                    self.inner_next();
                    let tok = if resumed {
                        Token::TemplateEnd(literal)
                    } else {
                        Token::String(literal)
                    };
                    return Ok((start, tok, self.get_location()));
                }
                '{' => {
                    self.inner_next();
                    self.templates.push(0);
                    let tok = if resumed {
                        Token::TemplateMiddle(literal)
                    } else {
                        Token::TemplateStart(literal)
                    };
                    return Ok((start, tok, self.get_location()));
                }
                '\\' => {
                    self.inner_next();
                    if let Some(&c) = self.inner_peek() {
                        match c {
                            // new line
                            'n' => {
                                self.inner_next();
                                literal.push('\n')
                            }
                            // horizontal tab
                            't' => {
                                self.inner_next();
                                literal.push('\t')
                            }
                            // carriage return
                            'r' => {
                                self.inner_next();
                                literal.push('\r')
                            }
                            '\\' | '"' | '{' | '}' => {
                                self.inner_next();
                                literal.push(c)
                            }
                            _ => {
                                self.warn(format!("unknown escape sequence '\\{}'", c));
                                self.inner_next();
                                literal.push('\\');
                                literal.push(c)
                            }
                        }
                    }
                }
                '\n' => {
                    self.inner_next();
                    self.advance_line();
                    literal.push(c)
                }
                _ => {
                    self.inner_next();
                    literal.push(c)
                }
            };
        }

        let mut quote_end = start;
        quote_end.col += 1;
        let help = if self.templates.is_empty() {
            "add a closing '\"' to end the string"
        } else {
            // The string that didn't end is most likely the rest of a
            // template whose `{` was never closed.
            "if this '\"' is meant to end a string, an expression in it is missing its '}'; \
             write '\\{' for a literal brace"
        };
        Err(LexicalError::new(start, quote_end, "unterminated string literal").help(help))
    }

    fn tok_number(&mut self, c: char, start: Location) -> LexResult {
        if c == '0' {
            let radix = match self.inner_peek() {
                Some('x') => Some((16, "hexadecimal")),
                Some('o') => Some((8, "octal")),
                Some('b') => Some((2, "binary")),
                _ => None,
            };
            if let Some((radix, name)) = radix {
                let prefix = self.inner_next().unwrap();
                return self.tok_radix_int(prefix, radix, name, start);
            }
        }

        let mut is_float = false;
        let after_dot = self.after_dot;
        let mut num_str = String::new();
        num_str.push(c);

        while let Some(c) = self.inner_peek() {
            match c {
                '0'..='9' => {
                    num_str.push(*c);
                    self.inner_next();
                }
                '.' if is_float || after_dot => break,
                '.' => {
                    let dot = self.get_location();
                    self.inner_next();
                    if self.inner_peek() == Some(&'.') {
                        // `0..10`: the number ends, and a `..` follows.
                        self.inner_next();
                        self.pending = Some((dot, Token::DotDot, self.get_location()));
                        break;
                    }
                    if self
                        .inner_peek()
                        .is_some_and(|c| c.is_ascii_alphabetic() || *c == '_')
                    {
                        // `3.min(9)`: a method call on the int, not a float.
                        self.pending = Some((dot, Token::Dot, self.get_location()));
                        break;
                    }
                    num_str.push('.');
                    is_float = true;
                }
                '_' => {
                    self.inner_next();
                }
                _ => break,
            }
        }

        if is_float {
            match num_str.parse::<f64>() {
                Ok(num) => Ok((start, Token::Float(num), self.get_location())),
                Err(_) => Err(LexicalError::new(
                    start,
                    self.get_location(),
                    format!("invalid float literal '{}'", num_str),
                )),
            }
        } else {
            match num_str.parse::<i64>() {
                Ok(num) => Ok((start, Token::Int(num), self.get_location())),
                Err(_) => Err(LexicalError::new(
                    start,
                    self.get_location(),
                    format!("integer literal '{}' is too large", num_str),
                )
                .help(format!(
                    "int values must be between {} and {}",
                    i64::MIN,
                    i64::MAX
                ))),
            }
        }
    }

    /// An int written with a `0x`, `0o` or `0b` prefix, already read, and
    /// digits that may be separated by `_`.
    fn tok_radix_int(
        &mut self,
        prefix: char,
        radix: u32,
        name: &str,
        start: Location,
    ) -> LexResult {
        let mut digits = String::new();
        let mut bad_digit = None;
        // Letters and digits run on in the literal, so `0b12` reports its `2`
        // rather than lexing as `0b1` then `2`.
        while let Some(&c) = self.inner_peek() {
            match c {
                '_' => {}
                c if c.is_ascii_alphanumeric() => {
                    if c.is_digit(radix) {
                        digits.push(c);
                    } else if bad_digit.is_none() {
                        bad_digit = Some(c);
                    }
                }
                _ => break,
            }
            self.inner_next();
        }
        let end = self.get_location();

        if let Some(c) = bad_digit {
            return Err(LexicalError::new(
                start,
                end,
                format!("invalid digit '{c}' in {name} literal"),
            ));
        }
        if digits.is_empty() {
            return Err(
                LexicalError::new(start, end, format!("{name} literal has no digits"))
                    .help(format!("write digits after the '0{prefix}' prefix")),
            );
        }
        match i64::from_str_radix(&digits, radix) {
            Ok(num) => Ok((start, Token::Int(num), end)),
            Err(_) => Err(LexicalError::new(
                start,
                end,
                format!("integer literal '0{prefix}{digits}' is too large"),
            )
            .help(format!(
                "int values must be between {} and {}",
                i64::MIN,
                i64::MAX
            ))),
        }
    }

    fn tok_keyword_or_ident(&mut self, c: char, start: Location) -> LexResult {
        let mut raw = String::new();
        raw.push(c);
        while let Some(&c) = self.inner_peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    self.inner_next();
                    raw.push(c);
                }
                _ => break,
            }
        }

        let keyword = match raw.as_str() {
            "let" => Some(Token::Let),
            "var" => Some(Token::Var),
            "fn" => Some(Token::Fn),
            "if" => Some(Token::If),
            "while" => Some(Token::While),
            "for" => Some(Token::For),
            "in" => Some(Token::In),
            "break" => Some(Token::Break),
            "continue" => Some(Token::Continue),
            "defer" => Some(Token::Defer),
            "else" => Some(Token::Else),
            "match" => Some(Token::Match),
            "return" => Some(Token::Return),
            "struct" => Some(Token::Struct),
            "enum" => Some(Token::Enum),
            "impl" => Some(Token::Impl),
            "import" => Some(Token::Import),
            "pub" => Some(Token::Pub),
            _ => None,
        };

        if let Some(kw) = keyword {
            Ok((start, kw, self.get_location()))
        } else {
            Ok((start, Token::Ident(raw.into()), self.get_location()))
        }
    }

    fn warn(&self, msg: String) {
        eprintln!("warning: {} (line {}, column {})", msg, self.line, self.col)
    }
}
/// Tokenize a whole source string, stopping at the first lexical error.
/// The returned tokens always end with `Token::Eof`.
pub fn tokenize(src: &str) -> Result<Vec<SrcSpan>, LexicalError> {
    tokenize_from_line(src, 1)
}

/// Like [`tokenize`], for code from `source`.
pub fn tokenize_source(src: &str, source: Source) -> Result<Vec<SrcSpan>, LexicalError> {
    let mut lexer = Lexer::new(src.chars());
    lexer.source = source;
    let mut tokens = Vec::new();
    loop {
        let span = lexer.token()?;
        let eof = span.1 == Token::Eof;
        tokens.push(span);
        if eof {
            return Ok(tokens);
        }
    }
}

/// Like [`tokenize`], but numbering lines from `first_line` (used by the REPL,
/// where each input is a new line of one growing session).
pub fn tokenize_from_line(src: &str, first_line: u32) -> Result<Vec<SrcSpan>, LexicalError> {
    let mut lexer = Lexer::new(src.chars());
    lexer.line = first_line;
    let mut tokens = Vec::new();
    loop {
        let span = lexer.token()?;
        let eof = span.1 == Token::Eof;
        tokens.push(span);
        if eof {
            return Ok(tokens);
        }
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = SrcSpan;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }

        match self.token() {
            Ok(t) => return Some(t),
            Err(e) => panic!("{}", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_literals_are_64_bit() {
        let tokens = tokenize("9223372036854775807").unwrap();
        assert_eq!(tokens[0].1, Token::Int(i64::MAX));

        let err = tokenize("9223372036854775808").unwrap_err();
        assert_eq!(
            err.message,
            "integer literal '9223372036854775808' is too large"
        );
        assert_eq!(
            err.help.as_deref(),
            Some("int values must be between -9223372036854775808 and 9223372036854775807")
        );
    }

    #[test]
    fn hex_octal_and_binary_int_literals() {
        use Token::*;
        assert_eq!(
            kinds("0xff 0xDead_Beef 0o17 0b1010_0101 0x0"),
            [
                Int(255),
                Int(0xdead_beef),
                Int(0o17),
                Int(0b1010_0101),
                Int(0),
                Eof
            ]
        );
        assert_eq!(kinds("0x7fff_ffff_ffff_ffff")[0], Int(i64::MAX));
        // A `.` or `..` after the digits ends the literal, as for decimals.
        assert_eq!(
            kinds("0x10.min(1)")[..3],
            [Int(16), Dot, Ident("min".into())]
        );
        assert_eq!(kinds("0b1..0b11")[..3], [Int(1), DotDot, Int(3)]);
    }

    #[test]
    fn bad_hex_octal_and_binary_int_literals() {
        let msg = |src: &str| tokenize(src).unwrap_err().message;
        assert_eq!(msg("0b102"), "invalid digit '2' in binary literal");
        assert_eq!(msg("0o8"), "invalid digit '8' in octal literal");
        assert_eq!(msg("0xfg"), "invalid digit 'g' in hexadecimal literal");
        assert_eq!(msg("0x"), "hexadecimal literal has no digits");
        assert_eq!(msg("0b_"), "binary literal has no digits");
        assert_eq!(
            msg("0x8000_0000_0000_0000"),
            "integer literal '0x8000000000000000' is too large"
        );
    }

    fn kinds(src: &str) -> Vec<Token> {
        tokenize(src)
            .unwrap()
            .into_iter()
            .map(|(_, t, _)| t)
            .collect()
    }

    #[test]
    fn template_strings_are_split_around_their_expressions() {
        use Token::*;
        assert_eq!(
            kinds(r#""a {x} b {f("c {y}")} d" "plain""#),
            [
                TemplateStart("a ".into()),
                Ident("x".into()),
                TemplateMiddle(" b ".into()),
                Ident("f".into()),
                LeftParen,
                TemplateStart("c ".into()),
                Ident("y".into()),
                TemplateEnd("".into()),
                RightParen,
                TemplateEnd(" d".into()),
                String("plain".into()),
                Eof,
            ]
        );
        // Braces inside the expression nest; escaped ones are text.
        assert_eq!(
            kinds(r#""{P { x: 1 }} \{ \} \" }""#),
            [
                TemplateStart("".into()),
                Ident("P".into()),
                LeftBrace,
                Ident("x".into()),
                Colon,
                Int(1),
                RightBrace,
                TemplateEnd(" { } \" }".into()),
                Eof,
            ]
        );
        let err = tokenize(r#""a {x"#).unwrap_err();
        assert_eq!(err.message, "unterminated expression in a template string");
    }

    #[test]
    fn a_dot_and_a_name_after_a_number_is_a_method_call() {
        use Token::*;
        assert_eq!(
            kinds("3.min(x) 2.5 1..2"),
            [
                Int(3),
                Dot,
                Ident("min".into()),
                LeftParen,
                Ident("x".into()),
                RightParen,
                Float(2.5),
                Int(1),
                DotDot,
                Int(2),
                Eof,
            ]
        );
    }
}
