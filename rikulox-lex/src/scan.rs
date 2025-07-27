use std::{iter::Peekable, str::CharIndices};

use rikulox_ast::{
    span::Span,
    token::{Keyword, Token, TokenKind},
};
use string_interner::{StringInterner, backend::StringBackend};

use crate::error::{ScanError, ScanErrorKind};

pub struct Scanner<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    string_interner: &'src mut StringInterner<StringBackend>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    start_line: usize,
    start_column: usize,
}

pub struct ScanTokens {
    pub tokens: Vec<Token>,
    pub eof_span: Span,
    pub errors: Vec<ScanError>,
}

impl<'src> Scanner<'src> {
    pub fn new(
        source: &'src str,
        string_interner: &'src mut StringInterner<StringBackend>,
    ) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            string_interner,
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_line: 1,
            start_column: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> ScanTokens {
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

    fn scan_token(&mut self) -> Option<Result<Token, ScanError>> {
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
            c if unicode_ident::is_xid_start(c) || c == '_' => match self.identifier() {
                Ok(token) => token,
                Err(error) => return Some(Err(error)),
            },
            _ => {
                return Some(Err(
                    self.make_error(ScanErrorKind::UnexpectedCharacter(char))
                ));
            }
        };

        Some(Ok(token))
    }

    fn number(&mut self) -> Result<Token, ScanError> {
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

    fn identifier(&mut self) -> Result<Token, ScanError> {
        while self.peek().is_some_and(unicode_ident::is_xid_continue) {
            self.advance();
        }

        match Keyword::try_from(&self.source[self.start..self.current]) {
            Ok(keyword) => Ok(self.make_token(TokenKind::Keyword(keyword))),
            Err(()) => {
                let symbol = self
                    .string_interner
                    .get_or_intern(&self.source[self.start..self.current]);
                Ok(self.make_token(TokenKind::Identifier(symbol)))
            }
        }
    }

    fn string(&mut self) -> Result<Token, ScanError> {
        while self.peek().is_some_and(|c| c != '"') {
            self.advance();
        }

        if self.peek().is_none() {
            return Err(self.make_error(ScanErrorKind::UnterminatedString));
        }

        // Advance the closing quote
        self.advance();

        let sym = self
            .string_interner
            .get_or_intern(&self.source[self.start + 1..self.current - 1]);
        Ok(self.make_token(TokenKind::String(sym)))
    }

    fn is_comment(&mut self) -> bool {
        self.peek()
            .is_some_and(|c| c == '/' && self.peek_next().is_some_and(|c| c == '/'))
    }

    /// Skips whitespace and comments.
    fn skip(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace()) || self.is_comment() {
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

    fn make_token(&self, kind: TokenKind) -> Token {
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
