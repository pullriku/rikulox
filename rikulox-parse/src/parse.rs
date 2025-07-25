use std::iter::Peekable;

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, UnaryOp},
    span::Span,
    stmt::{Stmt, StmtKind},
    token::{Keyword, Token, TokenKind},
};

use crate::error::{ExpectedItem, ParseError, ParseErrorKind};

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

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            stmts.push(self.declaration()?);
        }
        Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let &Token { kind, .. } = self.peek_or_err(ExpectedItem::Stmt)?;
        match kind {
            TokenKind::Keyword(Keyword::Var) => {
                let var_span = self.advance().unwrap().span;
                self.var_decl(var_span)
            }
            _ => self.statement(),
        }
    }

    fn var_decl(&mut self, var_span: Span) -> Result<Stmt, ParseError> {
        let name_symbol = match self.peek_or_err(ExpectedItem::Ident)? {
            &Token {
                kind: TokenKind::Identifier(name),
                ..
            } => {
                self.advance().unwrap();
                name
            }
            unexpected => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: ExpectedItem::Ident,
                        found: unexpected.kind,
                    },
                    span: unexpected.span,
                });
            }
        };

        let init = match self.peek() {
            Some(Token {
                kind: TokenKind::Equal,
                ..
            }) => {
                self.advance().unwrap();
                Some(self.expression()?)
            }

            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => None,

            // それ以外はエラー
            Some(unexpected) => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: ExpectedItem::Token(TokenKind::Equal),
                        found: unexpected.kind,
                    },
                    span: unexpected.span,
                });
            }

            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEof {
                        expected: ExpectedItem::Token(TokenKind::Semicolon),
                    },
                    span: self.eof_span,
                });
            }
        };

        // 最後にセミコロンを必ず消費
        let semi = self.consume(&TokenKind::Semicolon)?;

        Ok(Stmt {
            kind: StmtKind::Var {
                name: Identifier {
                    symbol: name_symbol,
                },
                init,
            },
            span: var_span.with_end_from(semi.span),
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let Token { kind, .. } = self.peek_or_err(ExpectedItem::Stmt)?;
        let kind = *kind;

        match kind {
            TokenKind::Keyword(Keyword::Print) => {
                let print_span = self.advance().unwrap().span;
                self.print_statement(print_span)
            }
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self, print_span: Span) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        let semi = self.consume(&TokenKind::Semicolon)?;
        Ok(Stmt {
            kind: StmtKind::Print(expr),
            span: print_span.with_end_from(semi.span),
        })
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        let expr_span = expr.span;
        let semi = self.consume(&TokenKind::Semicolon)?;
        Ok(Stmt {
            kind: StmtKind::Expression(expr),
            span: expr_span.with_end_from(semi.span),
        })
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        let expr_span = expr.span;

        while let Some(token) = self.peek()
            && matches!(token.kind, TokenKind::EqualEqual | TokenKind::BangEqual)
        {
            let token = self.advance().unwrap();

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

        while let Some(token) = self.peek()
            && matches!(
                token.kind,
                TokenKind::Greater
                    | TokenKind::GreaterEqual
                    | TokenKind::Less
                    | TokenKind::LessEqual
            )
        {
            let token = self.advance().unwrap();

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

        while let Some(token) = self.peek()
            && matches!(token.kind, TokenKind::Plus | TokenKind::Minus)
        {
            let token = self.advance().unwrap();

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

        while let Some(token) = self.peek()
            && matches!(token.kind, TokenKind::Star | TokenKind::Slash)
        {
            let token = self.advance().unwrap();

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
        let Token { kind, .. } = self.peek_or_err(ExpectedItem::Expr)?;
        let kind = *kind;

        match kind {
            TokenKind::Bang | TokenKind::Minus => {
                let token = self.advance().unwrap();
                let right = Box::new(self.unary()?);
                let right_span = right.span;
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::try_from(token.kind).unwrap(),
                        right,
                    },
                    span: token.span.with_end_from(right_span),
                })
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.advance_or_err(ExpectedItem::Expr)?;

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
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken {
                            expected: ExpectedItem::Expr,
                            found: token.kind,
                        },
                        span: token.span,
                    });
                }
            },
            TokenKind::Identifier(symbol) => Expr {
                kind: ExprKind::Variable(Identifier { symbol }),
                span: token.span,
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
                let r_paren = self.consume(&TokenKind::RParen)?;
                Expr {
                    kind: ExprKind::Grouping(expr),
                    span: token.span.with_end_from(r_paren.span),
                }
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: ExpectedItem::Expr,
                        found: token.kind,
                    },
                    span: token.span,
                });
            }
        };

        Ok(expr)
    }

    fn consume(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        if self.check_kind(kind) {
            return self.advance().ok_or_else(|| unreachable!());
        }

        Err(ParseError {
            kind: ParseErrorKind::UnexpectedToken {
                expected: ExpectedItem::Token(*kind),
                found: self.peek_or_err(ExpectedItem::Token(*kind))?.kind,
            },
            span: self.peek_span(),
        })
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

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_or_err(&mut self, expected: ExpectedItem) -> Result<&Token, ParseError> {
        let eof_span = self.eof_span;
        self.peek().ok_or(ParseError {
            kind: ParseErrorKind::UnexpectedEof { expected },
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

    fn advance_or_err(&mut self, expected: ExpectedItem) -> Result<Token, ParseError> {
        let Some(token) = self.advance() else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof { expected },
                span: self.eof_span,
            });
        };

        Ok(token)
    }
}
