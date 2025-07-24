use std::iter::Peekable;

use rikulox_ast::{
    ast::{BinOp, Expr, ExprKind, Literal, UnaryOp},
    span::Span,
    token::{Keyword, Token, TokenKind},
};

use crate::error::{ParseError, ParseErrorKind};

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    eof_span: Span,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: Peekable<I>, eof_span: Span) -> Self {
        Self { tokens, eof_span }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        let expr_span = expr.span;

        while matches!(
            self.peek_or_err()?.kind,
            TokenKind::EqualEqual | TokenKind::BangEqual
        ) {
            let token = self.advance_or_err()?;

            let op = BinOp::try_from(token.kind).unwrap();

            let right = self.comparison()?;
            let right_span = right.span;

            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span: expr_span.with_end_from(right_span),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        let expr_span = expr.span;

        while matches!(
            self.peek_or_err()?.kind,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        ) {
            let token = self.advance_or_err()?;

            let op = BinOp::try_from(token.kind).unwrap();

            let right = self.term()?;
            let right_span = right.span;

            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span: expr_span.with_end_from(right_span),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        let expr_span = expr.span;

        while matches!(self.peek_or_err()?.kind, TokenKind::Plus | TokenKind::Minus) {
            let token = self.advance_or_err()?;

            let op = BinOp::try_from(token.kind).unwrap();

            let right = self.factor()?;
            let right_span = right.span;

            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span: expr_span.with_end_from(right_span),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        let expr_span = expr.span;

        while matches!(self.peek_or_err()?.kind, TokenKind::Star | TokenKind::Slash) {
            let token = self.advance_or_err()?;

            let op = BinOp::try_from(token.kind).unwrap();

            let right = self.unary()?;
            let right_span = right.span;

            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span: expr_span.with_end_from(right_span),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        let Token { kind, .. } = self.peek_or_err()?;
        let kind = *kind;

        match kind {
            TokenKind::Bang | TokenKind::Minus => {
                let right = Box::new(self.unary()?);
                let right_span = right.span;
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::try_from(kind).unwrap(),
                        right,
                    },
                    span: self.advance_or_err()?.span.with_end_from(right_span),
                })
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance_or_err()?;

        let expr = match token.kind {
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::False => Expr {
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: token.span,
                },
                Keyword::True => Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: token.span,
                },
                Keyword::Nil => Expr {
                    kind: ExprKind::Literal(Literal::Nil),
                    span: token.span,
                },
                _ => unreachable!(),
            },
            TokenKind::Number(number) => Expr {
                kind: ExprKind::Literal(Literal::Number(number)),
                span: token.span,
            },
            TokenKind::String(string) => Expr {
                kind: ExprKind::Literal(Literal::String(string)),
                span: token.span,
            },
            TokenKind::LParen => {
                let expr = Box::new(self.expression()?);
                let r_paren = match self.consume(&TokenKind::RParen) {
                    Some(token) => token,
                    None => {
                        return Err(ParseError {
                            kind: ParseErrorKind::RParenNotFound,
                            span: self.peek_span(),
                        });
                    }
                };
                Expr {
                    kind: ExprKind::Grouping(expr),
                    span: token.span.with_end_from(r_paren.span),
                }
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(token.kind),
                    span: token.span,
                });
            }
        };

        Ok(expr)
    }

    fn check_kind(&mut self, kind: &TokenKind) -> bool {
        self.peek()
            .map(|token| match token.kind {
                TokenKind::Number(_) | TokenKind::String(_) | TokenKind::Identifier(_) => {
                    std::mem::discriminant(&token.kind) == std::mem::discriminant(kind)
                }

                _ => &token.kind == kind,
            })
            .unwrap_or(false)
    }

    fn consume(&mut self, kind: &TokenKind) -> Option<Token> {
        if self.check_kind(kind) {
            return self.advance();
        }

        None
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_or_err(&mut self) -> Result<&Token, ParseError> {
        let eof_span = self.eof_span;
        self.peek().ok_or(ParseError {
            kind: ParseErrorKind::UnexpectedEof,
            span: eof_span,
        })
    }

    fn peek_span(&mut self) -> Span {
        let peeked = self.peek();

        match peeked {
            Some(token) => token.span,
            None => self.eof_span,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn advance_or_err(&mut self) -> Result<Token, ParseError> {
        let Some(token) = self.advance() else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof,
                span: self.eof_span,
            });
        };

        Ok(token)
    }
}
