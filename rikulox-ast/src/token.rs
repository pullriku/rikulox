use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub lex: &'src str,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenKind<'src> {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(&'src str),
    String(&'src str),
    Number(f64),
    Keyword(Keyword),
    LineComment(&'src str),
}

#[derive(Debug, Clone)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl TryFrom<&str> for Keyword {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let keyword = match value {
            "and" => Keyword::And,
            "class" => Keyword::Class,
            "else" => Keyword::Else,
            "false" => Keyword::False,
            "for" => Keyword::For,
            "fun" => Keyword::Fun,
            "if" => Keyword::If,
            "nil" => Keyword::Nil,
            "or" => Keyword::Or,
            "print" => Keyword::Print,
            "return" => Keyword::Return,
            "super" => Keyword::Super,
            "this" => Keyword::This,
            "true" => Keyword::True,
            "var" => Keyword::Var,
            "while" => Keyword::While,
            _ => return Err(()),
        };

        Ok(keyword)
    }
}
