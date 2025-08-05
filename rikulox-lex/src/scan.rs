use std::{iter::Peekable, str::CharIndices};

use rikulox_ast::{
    span::Span,
    token::{Keyword, Token, TokenKind},
};

use crate::error::{ScanError, ScanErrorKind};

pub struct Scanner<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    start_line: usize,
    start_column: usize,
}

pub struct ScanTokens<'src> {
    pub tokens: Vec<Token<'src>>,
    pub eof_span: Span,
    pub errors: Vec<ScanError>,
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_line: 1,
            start_column: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> ScanTokens<'src> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some(token) = self.scan_token() {
            match token {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error),
            }
        }

        let eof_span = self.make_span();

        ScanTokens {
            tokens,
            eof_span,
            errors,
        }
    }

    fn scan_token(&mut self) -> Option<Result<Token<'src>, ScanError>> {
        self.skip();
        self.start = self.current;
        self.start_line = self.line;
        self.start_column = self.column;

        let char = self.advance()?;
        let token = match char {
            '(' => self.make_token(TokenKind::LParen),
            ')' => self.make_token(TokenKind::RParen),
            '{' => self.make_token(TokenKind::LBrace),
            '}' => self.make_token(TokenKind::RBrace),
            ',' => self.make_token(TokenKind::Comma),
            '.' => self.make_token(TokenKind::Dot),
            '-' => self.make_token(TokenKind::Minus),
            '+' => self.make_token(TokenKind::Plus),
            ';' => self.make_token(TokenKind::Semicolon),
            '*' => self.make_token(TokenKind::Star),
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::BangEqual)
                } else {
                    self.make_token(TokenKind::Bang)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqualEqual)
                } else {
                    self.make_token(TokenKind::Equal)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::LessEqual)
                } else {
                    self.make_token(TokenKind::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::GreaterEqual)
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }
            '/' => {
                if self.match_char('/') {
                    unreachable!("comments must be skipped");
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            '"' => match self.string() {
                Ok(token) => token,
                Err(error) => return Some(Err(error)),
            },
            digit if digit.is_ascii_digit() => match self.number() {
                Ok(token) => token,
                Err(error) => return Some(Err(error)),
            },
            c if unicode_ident::is_xid_start(c) || c == '_' => {
                match self.identifier() {
                    Ok(token) => token,
                    Err(error) => return Some(Err(error)),
                }
            }
            _ => {
                return Some(Err(
                    self.make_error(ScanErrorKind::UnexpectedCharacter(char))
                ));
            }
        };

        Some(Ok(token))
    }

    fn number(&mut self) -> Result<Token<'src>, ScanError> {
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        // floating point
        if self.peek().is_some_and(|c| c == '.')
            && self.peek_next().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance();

            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }

        Ok(self.make_token(TokenKind::Number(
            self.source[self.start..self.current].parse().unwrap(),
        )))
    }

    fn identifier(&mut self) -> Result<Token<'src>, ScanError> {
        while self.peek().is_some_and(unicode_ident::is_xid_continue) {
            self.advance();
        }

        match Keyword::try_from(&self.source[self.start..self.current]) {
            Ok(keyword) => Ok(self.make_token(TokenKind::Keyword(keyword))),
            Err(()) => Ok(self.make_token(TokenKind::Identifier(
                &self.source[self.start..self.current],
            ))),
        }
    }

    fn string(&mut self) -> Result<Token<'src>, ScanError> {
        while self.peek().is_some_and(|c| c != '"') {
            self.advance();
        }

        if self.peek().is_none() {
            return Err(self.make_error(ScanErrorKind::UnterminatedString));
        }

        // Advance the closing quote
        self.advance();

        Ok(self.make_token(TokenKind::String(
            &self.source[self.start + 1..self.current - 1],
        )))
    }

    fn is_comment(&mut self) -> bool {
        self.peek().is_some_and(|c| {
            c == '/' && self.peek_next().is_some_and(|c| c == '/')
        })
    }

    /// Skips whitespace and comments.
    fn skip(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace())
            || self.is_comment()
        {
            self.advance();
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        let Some(next) = self.peek() else {
            return false;
        };

        if next == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    fn make_token(&self, kind: TokenKind<'src>) -> Token<'src> {
        Token {
            kind,
            span: self.make_span(),
        }
    }

    fn make_span(&self) -> Span {
        Span {
            start: self.start,
            end: self.current,
        }
    }

    fn make_error(&self, kind: ScanErrorKind) -> ScanError {
        ScanError {
            kind,
            span: Span {
                start: self.start,
                end: self.current,
            },
        }
    }

    fn advance(&mut self) -> Option<char> {
        let (index, char) = self.chars.next()?;
        let next_index = index + char.len_utf8();
        self.current = next_index;

        if char == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        Some(char)
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|c| c.1)
    }

    fn peek_next(&mut self) -> Option<char> {
        self.source[self.current..].chars().nth(1)
    }
}

impl<'src> Iterator for Scanner<'src> {
    type Item = Result<Token<'src>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rikulox_ast::token::{Keyword, TokenKind};

    #[test]
    fn new_initializes_scanner() {
        let src = "abc";
        let scanner = Scanner::new(src);
        assert_eq!(scanner.source, src);
        assert_eq!(scanner.start, 0);
        assert_eq!(scanner.current, 0);
        assert_eq!(scanner.line, 1);
        assert_eq!(scanner.column, 1);
        assert_eq!(scanner.start_line, 1);
        assert_eq!(scanner.start_column, 1);
    }

    #[test]
    fn advance_and_peek_update_positions() {
        let mut scanner = Scanner::new("a\nb");
        assert_eq!(scanner.peek(), Some('a'));
        assert_eq!(scanner.peek_next(), Some('\n'));
        assert_eq!(scanner.advance(), Some('a'));
        assert_eq!(scanner.line, 1);
        assert_eq!(scanner.column, 2);
        assert_eq!(scanner.advance(), Some('\n'));
        assert_eq!(scanner.line, 2);
        assert_eq!(scanner.column, 0);
        assert_eq!(scanner.peek(), Some('b'));
        assert_eq!(scanner.advance(), Some('b'));
        assert_eq!(scanner.line, 2);
        assert_eq!(scanner.column, 1);
    }

    #[test]
    fn match_char_advances_on_match() {
        let mut scanner = Scanner::new("==");
        scanner.advance(); // consume first '='
        assert!(scanner.match_char('='));
        assert_eq!(scanner.current, 2);
        assert!(!scanner.match_char('='));
    }

    #[test]
    fn is_comment_detects_comment() {
        let mut scanner = Scanner::new("// comment");
        assert!(scanner.is_comment());
    }

    #[test]
    fn skip_ignores_whitespace_and_comments() {
        let mut scanner = Scanner::new("  \nabc");
        Scanner::skip(&mut scanner);
        assert_eq!(scanner.peek(), Some('a'));
        assert_eq!(scanner.line, 2);
    }

    #[test]
    fn string_literal_scanned() {
        let mut scanner = Scanner::new("\"foo\"");
        scanner.advance(); // opening quote
        let token = scanner.string().unwrap();
        assert_eq!(token.kind, TokenKind::String("foo"));
        assert_eq!(token.span.start, 0);
        assert_eq!(token.span.end, 5);
    }

    #[test]
    fn unterminated_string_returns_error() {
        let mut scanner = Scanner::new("\"foo");
        scanner.advance(); // opening quote
        let error = scanner.string().unwrap_err();
        assert!(matches!(error.kind, ScanErrorKind::UnterminatedString));
    }

    #[test]
    fn number_literal_scanned() {
        let mut scanner = Scanner::new("123");
        scanner.advance(); // first digit
        let token = scanner.number().unwrap();
        assert!(
            matches!(token.kind, TokenKind::Number(n) if (n - 123.0).abs() < f64::EPSILON)
        );
        assert_eq!(token.span.start, 0);
        assert_eq!(token.span.end, 3);
    }

    #[test]
    fn float_literal_scanned() {
        let mut scanner = Scanner::new("123.456");
        scanner.advance();
        let token = scanner.number().unwrap();
        assert!(
            matches!(token.kind, TokenKind::Number(n) if (n - 123.456).abs() < f64::EPSILON)
        );
    }

    #[test]
    fn identifier_and_keyword_scanned() {
        // identifier
        let mut scanner = Scanner::new("foo");
        scanner.advance();
        let token = scanner.identifier().unwrap();
        assert_eq!(token.kind, TokenKind::Identifier("foo"));

        // keyword
        let mut scanner = Scanner::new("for");
        scanner.advance();
        let token = scanner.identifier().unwrap();
        assert_eq!(token.kind, TokenKind::Keyword(Keyword::For));
    }

    #[test]
    fn scan_token_recognizes_punctuation() {
        let mut scanner = Scanner::new("()");
        let first = scanner.scan_token().unwrap().unwrap();
        assert_eq!(first.kind, TokenKind::LParen);
        let second = scanner.scan_token().unwrap().unwrap();
        assert_eq!(second.kind, TokenKind::RParen);
        assert!(scanner.scan_token().is_none());
    }

    #[test]
    fn scan_tokens_collects_tokens_and_errors() {
        let mut scanner = Scanner::new("( @ 123");
        let result = scanner.scan_tokens();
        assert_eq!(result.tokens[0].kind, TokenKind::LParen);
        assert!(
            result
                .tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::Number(_)))
        );
        assert_eq!(result.errors.len(), 1);
        assert!(matches!(
            result.errors[0].kind,
            ScanErrorKind::UnexpectedCharacter('@')
        ));
    }

    #[test]
    fn iterator_over_scanner_yields_tokens() {
        let mut scanner = Scanner::new("+-");
        assert_eq!(scanner.next().unwrap().unwrap().kind, TokenKind::Plus);
        assert_eq!(scanner.next().unwrap().unwrap().kind, TokenKind::Minus);
        assert!(scanner.next().is_none());
    }
}
