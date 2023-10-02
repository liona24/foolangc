use crate::string_pool::StringPool;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenId {
    EOF,

    Identifier,
    BuiltinIdentifier,

    LiteralByte,
    LiteralNumber,
    LiteralString,

    KeyByte,
    KeyInt,
    KeyStruct,
    KeyFunc,
    KeyIf,
    KeyElif,
    KeyElse,
    KeyOr,
    KeyAnd,
    KeyNot,
    KeyReturn,
    KeyLoop,
    KeyBreak,
    KeyContinue,

    Eq,
    Neq,
    Lt,

    OpenParen = '(' as isize,
    ClosingParen = ')' as isize,
    OpenCurly = '{' as isize,
    ClosingCurly = '}' as isize,
    OpenSquare = '[' as isize,
    ClosingSquare = ']' as isize,
    Dot = '.' as isize,
    Assign = '=' as isize,
    Comma = ',' as isize,
    Star = '*' as isize,
    Slash = '/' as isize,
    Plus = '+' as isize,
    Minus = '-' as isize,
    Reference = '&' as isize,
    Stop = ';' as isize,
}

impl TokenId {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "(" => Some(Self::OpenParen),
            ")" => Some(Self::ClosingParen),
            "{" => Some(Self::OpenCurly),
            "}" => Some(Self::ClosingCurly),
            "[" => Some(Self::OpenSquare),
            "]" => Some(Self::ClosingSquare),
            "." => Some(Self::Dot),
            "=" => Some(Self::Assign),
            "," => Some(Self::Comma),
            "*" => Some(Self::Star),
            "/" => Some(Self::Slash),
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            "&" => Some(Self::Reference),
            ";" => Some(Self::Stop),
            "==" => Some(Self::Eq),
            "!=" => Some(Self::Neq),
            "<" => Some(Self::Lt),
            "byte" => Some(Self::KeyByte),
            "int" => Some(Self::KeyInt),
            "struct" => Some(Self::KeyStruct),
            "func" => Some(Self::KeyFunc),
            "if" => Some(Self::KeyIf),
            "elif" => Some(Self::KeyElif),
            "else" => Some(Self::KeyElse),
            "or" => Some(Self::KeyOr),
            "and" => Some(Self::KeyAnd),
            "not" => Some(Self::KeyNot),
            "return" => Some(Self::KeyReturn),
            "loop" => Some(Self::KeyLoop),
            "break" => Some(Self::KeyBreak),
            "continue" => Some(Self::KeyContinue),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EOF => write!(f, "EOF"),
            Self::Identifier => write!(f, "identifier"),
            Self::BuiltinIdentifier => write!(f, "builtin"),
            Self::LiteralByte => write!(f, "byte_literal"),
            Self::LiteralNumber => write!(f, "number_literal"),
            Self::LiteralString => write!(f, "string_literal"),
            Self::KeyByte => write!(f, "byte"),
            Self::KeyInt => write!(f, "int"),
            Self::KeyStruct => write!(f, "struct"),
            Self::KeyFunc => write!(f, "func"),
            Self::KeyIf => write!(f, "if"),
            Self::KeyElif => write!(f, "elif"),
            Self::KeyElse => write!(f, "else"),
            Self::KeyOr => write!(f, "or"),
            Self::KeyAnd => write!(f, "and"),
            Self::KeyNot => write!(f, "not"),
            Self::KeyReturn => write!(f, "return"),
            Self::KeyLoop => write!(f, "loop"),
            Self::KeyBreak => write!(f, "break"),
            Self::KeyContinue => write!(f, "continue"),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::OpenParen => write!(f, "("),
            Self::ClosingParen => write!(f, ")"),
            Self::OpenCurly => write!(f, "{{"),
            Self::ClosingCurly => write!(f, "}}"),
            Self::OpenSquare => write!(f, "["),
            Self::ClosingSquare => write!(f, "]"),
            Self::Dot => write!(f, "."),
            Self::Assign => write!(f, "="),
            Self::Comma => write!(f, ","),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Reference => write!(f, "&"),
            Self::Stop => write!(f, ";"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NonTerminal {
    Start,

    Block,

    Statement,
    Statements,

    GlobalStatement,
    GlobalStatements,

    Return,

    Decl,

    Assign,

    Func,
    Params,

    Struct,
    StructFields,
    StructField,

    Call,
    Args,
    Arg,

    LogicExprL1,
    LogicExprL2,
    LogicExprL3,

    ExprL1,
    ExprL2,
    ExprL3,

    Type,
    Array,
    Pointer,

    Ref,
    Deref,
    AddrOf,
    Rhs,
    Lhs,

    Literal,

    If,
    Elif,
    Else,

    Loop,
}

impl std::fmt::Debug for NonTerminal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Start => write!(f, "$"),
            Self::Block => write!(f, "BLOCK"),
            Self::Statement => write!(f, "STATEMENT"),
            Self::Statements => write!(f, "STATEMENTS"),
            Self::GlobalStatement => write!(f, "GLOBAL_STATEMENT"),
            Self::GlobalStatements => write!(f, "GLOBAL_STATEMENTS"),
            Self::Return => write!(f, "RETURN"),
            Self::Decl => write!(f, "DECL"),
            Self::Assign => write!(f, "ASSIGN"),
            Self::Func => write!(f, "FUNC"),
            Self::Params => write!(f, "PARAMS"),
            Self::Struct => write!(f, "STRUCT"),
            Self::StructFields => write!(f, "STRUCT_FIELDS"),
            Self::StructField => write!(f, "STRUCT_FIELD"),
            Self::Call => write!(f, "CALL"),
            Self::Args => write!(f, "ARGS"),
            Self::Arg => write!(f, "ARG"),
            Self::LogicExprL1 => write!(f, "LOGIC_EXPR_L1"),
            Self::LogicExprL2 => write!(f, "LOGIC_EXPR_L2"),
            Self::LogicExprL3 => write!(f, "LOGIC_EXPR_L3"),
            Self::ExprL1 => write!(f, "EXPR_L1"),
            Self::ExprL2 => write!(f, "EXPR_L2"),
            Self::ExprL3 => write!(f, "EXPR_L3"),
            Self::Type => write!(f, "TYPE"),
            Self::Array => write!(f, "ARRAY"),
            Self::Pointer => write!(f, "POINTER"),
            Self::Ref => write!(f, "REF"),
            Self::Deref => write!(f, "DEREF"),
            Self::AddrOf => write!(f, "ADDR_OF"),
            Self::Rhs => write!(f, "RHS"),
            Self::Lhs => write!(f, "LHS"),
            Self::Literal => write!(f, "LITERAL"),
            Self::If => write!(f, "IF"),
            Self::Elif => write!(f, "ELIF"),
            Self::Else => write!(f, "ELSE"),
            Self::Loop => write!(f, "LOOP"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenRange {
    pub lineno: usize,
    pub begin: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub id: TokenId,
    pub meta: u64,
    pub range: Option<TokenRange>,
}

impl Token {
    pub fn pretty(&self, pool: Option<&StringPool>) -> String {
        if let Some(pool) = pool {
            match self.id {
                TokenId::Identifier => pool.get(self.meta as u32).to_string(),
                TokenId::BuiltinIdentifier => {
                    format!("@{}", pool.get(self.meta as u32))
                }
                TokenId::LiteralByte => {
                    format!("{} 0x{:016x}", self.meta, self.meta)
                }
                TokenId::LiteralNumber => {
                    format!("{} 0x{:016x}", self.meta, self.meta)
                }
                TokenId::LiteralString => {
                    format!("\"{}\"", pool.get(self.meta as u32).escape_debug())
                }
                _ => {
                    format!("{}", self.id)
                }
            }
        } else {
            format!("{}", self.id)
        }
    }

    pub fn eof() -> Self {
        Self {
            id: TokenId::EOF,
            meta: 0,
            range: None,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Token {}
impl Ord for Token {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl AsRef<Token> for Token {
    fn as_ref(&self) -> &Token {
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol {
    Terminal { token: Token },
    NonTerminal(NonTerminal),
}

impl Symbol {
    pub fn pretty(&self, pool: Option<&StringPool>) -> String {
        match self {
            Symbol::Terminal { token } => format!("{}", token.pretty(pool)),
            Symbol::NonTerminal(n) => format!("{:?}", n),
        }
    }
}
