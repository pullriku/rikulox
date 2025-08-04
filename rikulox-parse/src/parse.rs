use std::iter::Peekable;

use rikulox_ast::{
    expr::{BinOp, Expr, ExprKind, Identifier, Literal, LogicalOp, UnaryOp},
    id::IdGen,
    span::Span,
    stmt::{ClassDecl, FunctionDecl, Stmt, StmtKind},
    token::{Keyword, Token, TokenKind},
};

use crate::error::{ExpectedItem, ParseError, ParseErrorKind};

pub const MAX_ARGS: usize = u8::MAX as usize;

pub struct Parser<'src, I>
where
    I: Iterator<Item = Token<'src>>,
{
    tokens: Peekable<I>,
    eof_span: Span,
    id_gen: IdGen,
}

impl<'src, I> Parser<'src, I>
where
    I: Iterator<Item = Token<'src>>,
{
    pub fn new(tokens: Peekable<I>, eof_span: Span) -> Self {
        Self {
            tokens,
            eof_span,
            id_gen: IdGen::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt<'src>>, ParseError<'src>> {
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            stmts.push(self.declaration()?);
        }
        Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Stmt<'src>, ParseError<'src>> {
        let &Token { kind, .. } = self.peek_or_err(ExpectedItem::Stmt)?;
        match kind {
            TokenKind::Keyword(Keyword::Var) => {
                let var_span = self.advance().unwrap().span;
                self.var_decl(var_span)
            }
            TokenKind::Keyword(Keyword::Fun) => {
                let fun_span = self.advance().unwrap().span;
                let (fun, end_span) = self.function_decl(Some(fun_span))?;
                Ok(Stmt {
                    kind: StmtKind::Function(fun),
                    span: fun_span.with_end_from(end_span),
                    id: self.id_gen.next_id(),
                })
            }
            TokenKind::Keyword(Keyword::Class) => {
                let class_span = self.advance().unwrap().span;
                self.class_decl(class_span)
            }
            _ => self.statement(),
        }
    }

    fn var_decl(
        &mut self,
        var_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
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
            id: self.id_gen.next_id(),
        })
    }

    fn function_decl(
        &mut self,
        fun_span: Option<Span>,
    ) -> Result<(FunctionDecl<'src>, Span), ParseError<'src>> {
        let Token {
            kind: TokenKind::Identifier(ident),
            span: ident_span,
        } = self.consume(&TokenKind::Identifier(""))?
        else {
            unreachable!();
        };

        self.consume(&TokenKind::LParen)?;

        let mut params = Vec::new();

        if !self.check_kind(&TokenKind::RParen) {
            loop {
                let Token {
                    kind: TokenKind::Identifier(param_ident),
                    span: _,
                } = self.consume(&TokenKind::Identifier(""))?
                else {
                    unreachable!();
                };
                params.push(Identifier {
                    symbol: param_ident,
                });

                if self.check_kind(&TokenKind::Comma) {
                    self.advance().unwrap();
                } else {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RParen)?;

        self.consume(&TokenKind::LBrace)?;
        let (body, end_span) = self.block()?;

        Ok((
            FunctionDecl {
                name: Identifier { symbol: ident },
                params,
                body,
            },
            fun_span.unwrap_or(ident_span).with_end_from(end_span),
        ))
    }

    fn class_decl(
        &mut self,
        class_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        let name = match self.peek_or_err(ExpectedItem::Ident)? {
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

        self.consume(&TokenKind::LBrace)?;

        let mut methods = Vec::new();

        while let Some(token) = self.peek()
            && !matches!(token.kind, TokenKind::RBrace)
        {
            let (fun, _end_span) = self.function_decl(None)?;
            methods.push(fun);
        }

        let r_brace = self.consume(&TokenKind::RBrace)?;

        Ok(Stmt {
            kind: StmtKind::Class(ClassDecl {
                name: Identifier { symbol: name },
                methods,
            }),
            span: class_span.with_end_from(r_brace.span),
            id: self.id_gen.next_id(),
        })
    }

    fn statement(&mut self) -> Result<Stmt<'src>, ParseError<'src>> {
        let Token { kind, .. } = self.peek_or_err(ExpectedItem::Stmt)?;
        let kind = *kind;

        match kind {
            TokenKind::Keyword(Keyword::Print) => {
                let print_span = self.advance().unwrap().span;
                self.print_statement(print_span)
            }
            TokenKind::Keyword(Keyword::While) => {
                let while_span = self.advance().unwrap().span;
                self.while_statement(while_span)
            }
            TokenKind::Keyword(Keyword::If) => {
                let if_span = self.advance().unwrap().span;
                self.if_statement(if_span)
            }
            TokenKind::Keyword(Keyword::For) => {
                let for_span = self.advance().unwrap().span;
                self.for_statement(for_span)
            }
            TokenKind::Keyword(Keyword::Return) => {
                let return_span = self.advance().unwrap().span;
                self.return_statement(return_span)
            }
            TokenKind::LBrace => {
                let l_brace_span = self.advance().unwrap().span;
                self.block_statement(l_brace_span)
            }
            _ => self.expression_statement(),
        }
    }

    fn print_statement(
        &mut self,
        print_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        let expr = self.expression()?;
        let semi = self.consume(&TokenKind::Semicolon)?;
        Ok(Stmt {
            kind: StmtKind::Print(expr),
            span: print_span.with_end_from(semi.span),
            id: self.id_gen.next_id(),
        })
    }

    fn for_statement(
        &mut self,
        for_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        self.consume(&TokenKind::LParen)?;

        let init = match self.peek() {
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => {
                self.advance().unwrap();
                None
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Var),
                ..
            }) => {
                let var_span = self.advance().unwrap().span;
                Some(self.var_decl(var_span)?)
            }
            _ => Some(self.expression_statement()?),
        };

        let condition = match self.peek() {
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => None,
            _ => Some(self.expression()?),
        };

        self.consume(&TokenKind::Semicolon)?;

        let increment = match self.peek() {
            Some(Token {
                kind: TokenKind::RParen,
                ..
            }) => None,
            _ => Some(self.expression()?),
        };

        self.consume(&TokenKind::RParen)?;

        let body = self.statement()?;

        let body_span = body.span;
        let all_span = for_span.with_end_from(body_span);
        let condition = condition.unwrap_or(Expr {
            kind: ExprKind::Literal(Literal::Bool(true)),
            span: for_span.with_end_from(all_span),
            id: self.id_gen.next_id(),
        });

        let body = match increment {
            Some(increment) => Stmt {
                kind: StmtKind::Block(vec![
                    body,
                    Stmt {
                        kind: StmtKind::Expression(increment),
                        span: body_span,
                        id: self.id_gen.next_id(),
                    },
                ]),
                span: all_span,
                id: self.id_gen.next_id(),
            },
            None => body,
        };

        let while_body = Stmt {
            kind: StmtKind::While {
                condition,
                body: Box::new(body),
            },
            span: all_span,
            id: self.id_gen.next_id(),
        };

        let body = match init {
            Some(init) => Stmt {
                kind: StmtKind::Block(vec![init, while_body]),
                span: all_span,
                id: self.id_gen.next_id(),
            },
            None => while_body,
        };

        Ok(body)
    }

    fn while_statement(
        &mut self,
        while_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        self.consume(&TokenKind::LParen)?;
        let condition = self.expression()?;
        self.consume(&TokenKind::RParen)?;

        let body = self.statement()?;

        let end_span = body.span;
        Ok(Stmt {
            kind: StmtKind::While {
                condition,
                body: Box::new(body),
            },
            span: while_span.with_end_from(end_span),
            id: self.id_gen.next_id(),
        })
    }

    fn if_statement(
        &mut self,
        if_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        self.consume(&TokenKind::LParen)?;
        let condition = self.expression()?;
        self.consume(&TokenKind::RParen)?;

        let then_branch = self.statement()?;
        let else_branch = match self.peek() {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Else),
                ..
            }) => {
                self.advance().unwrap();
                Some(self.statement()?)
            }
            _ => None,
        };

        let end_span = match &else_branch {
            Some(else_branch) => else_branch.span,
            None => then_branch.span,
        };

        Ok(Stmt {
            kind: StmtKind::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            },
            span: if_span.with_end_from(end_span),
            id: self.id_gen.next_id(),
        })
    }

    fn block_statement(
        &mut self,
        l_brace_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        let (stmts, r_brace_span) = self.block()?;
        Ok(Stmt {
            kind: StmtKind::Block(stmts),
            span: l_brace_span.with_end_from(r_brace_span),
            id: self.id_gen.next_id(),
        })
    }

    fn block(&mut self) -> Result<(Vec<Stmt<'src>>, Span), ParseError<'src>> {
        let mut stmts = Vec::new();

        while let Some(token) = self.peek()
            && !matches!(token.kind, TokenKind::RBrace)
        {
            stmts.push(self.declaration()?);
        }

        let r_brace = self.consume(&TokenKind::RBrace)?;

        Ok((stmts, r_brace.span))
    }

    fn return_statement(
        &mut self,
        return_span: Span,
    ) -> Result<Stmt<'src>, ParseError<'src>> {
        let value = match self.peek() {
            Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => {
                self.advance().unwrap();
                None
            }
            _ => {
                let expr = self.expression()?;
                self.consume(&TokenKind::Semicolon)?;
                Some(expr)
            }
        };

        Ok(Stmt {
            kind: StmtKind::Return(value),
            span: return_span,
            id: self.id_gen.next_id(),
        })
    }

    fn expression_statement(&mut self) -> Result<Stmt<'src>, ParseError<'src>> {
        let expr = self.expression()?;
        let expr_span = expr.span;
        let semi = self.consume(&TokenKind::Semicolon)?;
        Ok(Stmt {
            kind: StmtKind::Expression(expr),
            span: expr_span.with_end_from(semi.span),
            id: self.id_gen.next_id(),
        })
    }

    fn expression(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let expr = self.or()?;

        if let Some(token) = self.peek()
            && matches!(token.kind, TokenKind::Equal)
        {
            let token_eq = self.advance().unwrap();
            let value = self.assignment()?;

            if let ExprKind::Variable(ident) = expr.kind {
                Ok(Expr {
                    kind: ExprKind::Assign {
                        name: ident,
                        value: Box::new(value),
                    },
                    span: expr.span.with_end_from(token_eq.span),
                    id: self.id_gen.next_id(),
                })
            } else if let ExprKind::Get { left: object, name } = expr.kind {
                Ok(Expr {
                    kind: ExprKind::Set {
                        left: object,
                        name,
                        value: Box::new(value),
                    },
                    span: expr.span.with_end_from(token_eq.span),
                    id: self.id_gen.next_id(),
                })
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken {
                        expected: ExpectedItem::Ident,
                        found: token_eq.kind,
                    },
                    span: token_eq.span,
                })
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let mut expr = self.and()?;
        let expr_span = expr.span;

        while let Some(token) = self.peek()
            && matches!(token.kind, TokenKind::Keyword(Keyword::Or))
        {
            let token = self.advance().unwrap();
            let right = self.and()?;
            expr = Expr {
                kind: ExprKind::Logical {
                    left: Box::new(expr),
                    op: LogicalOp::Or,
                    right: Box::new(right),
                },
                span: expr_span.with_end_from(token.span),
                id: self.id_gen.next_id(),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let mut expr = self.equality()?;
        let expr_span = expr.span;

        while let Some(token) = self.peek()
            && matches!(token.kind, TokenKind::Keyword(Keyword::And))
        {
            let token = self.advance().unwrap();
            let right = self.equality()?;
            expr = Expr {
                kind: ExprKind::Logical {
                    left: Box::new(expr),
                    op: LogicalOp::And,
                    right: Box::new(right),
                },
                span: expr_span.with_end_from(token.span),
                id: self.id_gen.next_id(),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let mut expr = self.comparison()?;
        let expr_span = expr.span;

        while let Some(token) = self.peek()
            && matches!(
                token.kind,
                TokenKind::EqualEqual | TokenKind::BangEqual
            )
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
                id: self.id_gen.next_id(),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
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
                id: self.id_gen.next_id(),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
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
                id: self.id_gen.next_id(),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
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
                id: self.id_gen.next_id(),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
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
                    id: self.id_gen.next_id(),
                })
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let mut expr = self.primary()?;
        let expr_span = expr.span;

        loop {
            match self.peek().map(|token| token.kind) {
                Some(TokenKind::LParen) => {
                    let l_paren = self.advance().unwrap();
                    let (args, r_paren_span) = self.arguments(l_paren.span)?;

                    expr = Expr {
                        kind: ExprKind::Call {
                            callee: Box::new(expr),
                            args,
                        },
                        span: expr_span.with_end_from(r_paren_span),
                        id: self.id_gen.next_id(),
                    }
                }

                Some(TokenKind::Dot) => {
                    let _dot = self.advance().unwrap();
                    let Token {
                        kind: TokenKind::Identifier(ident),
                        span: ident_span,
                    } = self.consume(&TokenKind::Identifier(""))?
                    else {
                        unreachable!();
                    };

                    expr = Expr {
                        kind: ExprKind::Get {
                            left: Box::new(expr),
                            name: Identifier { symbol: ident },
                        },
                        span: expr_span.with_end_from(ident_span),
                        id: self.id_gen.next_id(),
                    }
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn arguments(
        &mut self,
        l_paren_span: Span,
    ) -> Result<(Vec<Expr<'src>>, Span), ParseError<'src>> {
        let mut args = Vec::new();

        if let Some(token) = self.peek()
            && !matches!(token.kind, TokenKind::RParen)
        {
            loop {
                args.push(self.expression()?);

                if self.check_kind(&TokenKind::Comma) {
                    self.advance().unwrap();
                } else {
                    break;
                }
            }
        }

        let r_paren =
            self.advance_or_err(ExpectedItem::Token(TokenKind::RParen))?;

        if args.len() >= MAX_ARGS {
            return Err(ParseError {
                kind: ParseErrorKind::TooManyArguments,
                span: l_paren_span.with_end_from(r_paren.span),
            });
        }

        Ok((args, r_paren.span))
    }

    fn primary(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let token = self.advance_or_err(ExpectedItem::Expr)?;

        let expr = match token.kind {
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::False => Expr {
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: token.span,
                    id: self.id_gen.next_id(),
                },
                Keyword::True => Expr {
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: token.span,
                    id: self.id_gen.next_id(),
                },
                Keyword::Nil => Expr {
                    kind: ExprKind::Literal(Literal::Nil),
                    span: token.span,
                    id: self.id_gen.next_id(),
                },
                Keyword::This => Expr {
                    kind: ExprKind::This,
                    span: token.span,
                    id: self.id_gen.next_id(),
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
                id: self.id_gen.next_id(),
            },
            TokenKind::Number(number) => Expr {
                kind: ExprKind::Literal(Literal::Number(number)),
                span: token.span,
                id: self.id_gen.next_id(),
            },
            TokenKind::String(string) => Expr {
                kind: ExprKind::Literal(Literal::String(string)),
                span: token.span,
                id: self.id_gen.next_id(),
            },
            TokenKind::LParen => {
                let expr = Box::new(self.expression()?);
                let r_paren = self.consume(&TokenKind::RParen)?;
                Expr {
                    kind: ExprKind::Grouping(expr),
                    span: token.span.with_end_from(r_paren.span),
                    id: self.id_gen.next_id(),
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

    fn consume(
        &mut self,
        kind: &TokenKind<'src>,
    ) -> Result<Token<'src>, ParseError<'src>> {
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
                TokenKind::Number(_)
                | TokenKind::String(_)
                | TokenKind::Identifier(_) => {
                    std::mem::discriminant(&token.kind)
                        == std::mem::discriminant(kind)
                }

                _ => &token.kind == kind,
            })
            .unwrap_or(false)
    }

    fn peek(&mut self) -> Option<&Token<'src>> {
        self.tokens.peek()
    }

    fn peek_or_err(
        &mut self,
        expected: ExpectedItem<'src>,
    ) -> Result<&Token<'src>, ParseError<'src>> {
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

    fn advance(&mut self) -> Option<Token<'src>> {
        self.tokens.next()
    }

    fn advance_or_err(
        &mut self,
        expected: ExpectedItem<'src>,
    ) -> Result<Token<'src>, ParseError<'src>> {
        let Some(token) = self.advance() else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof { expected },
                span: self.eof_span,
            });
        };

        Ok(token)
    }
}
