use std::collections::BTreeMap;

use crate::error::CompilerError;
use crate::parse_types::{NonTerminal, Symbol, Token, TokenId, TokenRange};
use crate::parsing::{Grammar, Lexer, Rule};
use crate::string_pool::StringPool;

pub fn create_grammar(_strings: &mut StringPool) -> Grammar {
    macro_rules! T {
        ($id:expr) => {
            Symbol::Terminal {
                token: Token {
                    id: $id,
                    meta: 0,
                    range: None,
                },
            }
        };
        ($_marker:tt $id:expr) => {
            Symbol::Terminal {
                token: Token {
                    id: TokenId::from_str($id).unwrap(),
                    meta: 0,
                    range: None,
                },
            }
        };
    }

    macro_rules! S {
        ($nt:tt) => {
            Symbol::NonTerminal(NonTerminal::$nt)
        };
    }

    let mut rules = BTreeMap::new();
    macro_rules! rule {
        ($left:expr => $($right:expr),+) => {{
            rules.insert(Symbol::NonTerminal($left), vec![$(Rule { left: $left, right: $right}, )+]);
        }}
    }

    rule!(
        NonTerminal::Start => vec![S!(GlobalStatements)]
    );
    rule!(NonTerminal::GlobalStatements =>
        vec![S!(GlobalStatement)],
        vec![S!(GlobalStatements), S!(GlobalStatement)]
    );
    rule!(
        NonTerminal::Statements =>
            vec![S!(Statement)],
            vec![S!(Statements), S!(Statement)]
    );
    rule!(
        NonTerminal::Block =>
            vec![T!(_ "{"), S!(Statements), T!(_ "}")],
            vec![T!(_ "{"), T!(_ "}")]
    );

    rule!(
        NonTerminal::GlobalStatement =>
            vec![S!(Decl), T!(_ ";")],
            vec![S!(Assign), T!(_ ";")],
            vec![S!(ExprL1), T!(_ ";")],
            vec![S!(LogicExprL1), T!(_ ";")],
            vec![S!(Func)],
            vec![S!(If)],
            vec![S!(Loop)],
            vec![S!(Struct)]
    );

    rule!(
        NonTerminal::Statement =>
            vec![S!(Decl), T!(_ ";")],
            vec![S!(Assign), T!(_ ";")],
            vec![S!(ExprL1), T!(_ ";")],
            vec![S!(LogicExprL1), T!(_ ";")],
            vec![S!(If)],
            vec![S!(Loop)],
            vec![S!(Return), T!(_ ";")],
            vec![T!(_ "break"), T!(_ ";")],
            vec![T!(_ "continue"), T!(_ ";")]
    );
    rule!(
        NonTerminal::Return =>
            vec![T!(_ "return")],
            vec![T!(_ "return"), S!(ExprL1)],
            vec![T!(_ "return"), S!(LogicExprL1)]
    );

    rule!(
        NonTerminal::Array =>
            vec![S!(Type), T!(_ "["), T!(TokenId::LiteralNumber), T!(_ "]")]
    );
    rule!(
        NonTerminal::Pointer =>
            vec![S!(Type), T!(_ "*")]
    );

    rule!(
        NonTerminal::Type =>
            vec![S!(Array)],
            vec!(S!(Pointer)),
            vec![T!(_ "struct"), T!(TokenId::Identifier)],
            vec![T!(_ "int")],
            vec![T!(_ "byte")]
    );

    rule!(
        NonTerminal::Params =>
            vec![S!(Type), T!(TokenId::Identifier)],
            vec![S!(Params), T!(_ ","), S!(Type), T!(TokenId::Identifier)]
    );

    rule!(
        NonTerminal::Decl =>
            vec![S!(Type), T!(TokenId::Identifier), T!(_ "="), S!(ExprL1)],
            vec![S!(Type), T!(TokenId::Identifier), T!(_ "="), S!(LogicExprL1), T!(_ ";")]
    );

    rule!(
        NonTerminal::Func =>
            vec![T!(_ "func"), T!(TokenId::Identifier), T!(_ "("), S!(Params), T!(_ ")"), T!(_ "="), S!(Type), S!(Block)],
            vec![T!(_ "func"), T!(TokenId::Identifier), T!(_ "("), S!(Params), T!(_ ")"), S!(Block)],
            vec![T!(_ "func"), T!(TokenId::Identifier), T!(_ "("), T!(_ ")"), T!(_ "="), S!(Type), S!(Block)],
            vec![T!(_ "func"), T!(TokenId::Identifier), T!(_ "("), T!(_ ")"), S!(Block)]
    );

    rule!(
        NonTerminal::Struct =>
            vec![T!(_ "struct"), T!(TokenId::Identifier), T!(_ "{"), S!(StructFields), T!(_ "}")]
    );

    rule!(
        NonTerminal::StructFields =>
            vec![S!(StructField)],
            vec![S!(StructFields), S!(StructField)]
    );

    rule!(
        NonTerminal::StructField =>
            vec![S!(Type), T!(TokenId::Identifier), T!(_ ";")]
    );

    rule!(
        NonTerminal::Literal =>
            vec![T!(TokenId::LiteralByte)],
            vec![T!(TokenId::LiteralNumber)],
            vec![T!(TokenId::LiteralString)]
    );

    rule!(
        NonTerminal::ExprL1 =>
            vec![S!(ExprL2)],
            vec![S!(ExprL1), T!(_ "+"), S!(ExprL2)],
            vec![S!(ExprL1), T!(_ "-"), S!(ExprL2)]
    );

    rule!(
        NonTerminal::ExprL2 =>
            vec![S!(ExprL3)],
            vec![S!(ExprL2), T!(_ "*"), S!(ExprL3)],
            vec![S!(ExprL2), T!(_ "/"), S!(ExprL3)]
    );

    rule!(
        NonTerminal::ExprL3 =>
            vec![S!(Rhs)],
            vec![S!(AddrOf)],
            vec![S!(Literal)],
            vec![T!(_ "-"), S!(ExprL3)],
            vec![T!(_ "("), S!(ExprL1), T!(_ ")")],
            vec![S!(Call)]
    );

    rule!(
        NonTerminal::LogicExprL1 =>
            vec![S!(LogicExprL2)],
            vec![S!(LogicExprL1), T!(_ "or"), S!(LogicExprL2)]
    );

    rule!(
        NonTerminal::LogicExprL2 =>
            vec![S!(LogicExprL3)],
            vec![S!(LogicExprL2), T!(_ "and"), S!(LogicExprL3)]
    );

    rule!(
        NonTerminal::LogicExprL3 =>
            vec![T!(_ "not"), T!(_ "("), S!(LogicExprL1), T!(_ ")")],
            vec![S!(ExprL1), T!(_ "<"), S!(ExprL1)],
            vec![S!(ExprL1), T!(_ "=="), S!(ExprL1)],
            vec![S!(ExprL1), T!(_ "!="), S!(ExprL1)]
    );

    rule!(
        NonTerminal::Ref =>
            vec![T!(TokenId::Identifier)],
            vec![S!(Ref), T!(_ "."), T!(TokenId::Identifier)],
            vec![S!(Ref), T!(_ "["), S!(ExprL1), T!(_ "]")]
    );

    rule!(
        NonTerminal::Deref =>
            vec![T!(_ "*"), S!(Ref)],
            vec![T!(_ "*"), S!(Deref)]
    );

    rule!(
        NonTerminal::AddrOf =>
            vec![T!(_ "&"), S!(Ref)],
            vec![T!(_ "&"), T!(TokenId::LiteralString)]
    );

    rule!(
        NonTerminal::Lhs =>
            vec![S!(Deref)],
            vec![S!(Ref)]
    );

    rule!(
        NonTerminal::Rhs =>
            vec![S!(Deref)],
            vec![S!(Ref)]
    );

    rule!(
        NonTerminal::Assign =>
            vec![S!(Lhs), T!(_ "="), S!(ExprL1)],
            vec![S!(Lhs), T!(_ "="), S!(LogicExprL1)]
    );

    rule!(
        NonTerminal::Call =>
            vec![T!(TokenId::Identifier), T!(_ "("), S!(Args), T!(_ ")")],
            vec![T!(TokenId::Identifier), T!(_ "("), T!(_ ")")],
            vec![T!(TokenId::BuiltinIdentifier), T!(_ "("), S!(Args), T!(_ ")")],
            vec![T!(TokenId::BuiltinIdentifier), T!(_ "("), T!(_ ")")]
    );

    rule!(
        NonTerminal::Args =>
            vec![S!(Arg)],
            vec![S!(Args), T!(_ ","), S!(Arg)]
    );
    rule!(
        NonTerminal::Arg =>
            vec![S!(ExprL1)],
            vec![S!(LogicExprL1)]
    );

    rule!(
        NonTerminal::If =>
            vec![T!(_ "if"), S!(LogicExprL1), S!(Block)],
            vec![T!(_ "if"), S!(LogicExprL1), S!(Block), S!(Elif)],
            vec![T!(_ "if"), S!(LogicExprL1), S!(Block), S!(Else)]
    );
    rule!(
        NonTerminal::Elif =>
            vec![T!(_ "elif"), S!(LogicExprL1), S!(Block)],
            vec![T!(_ "elif"), S!(LogicExprL1), S!(Block), S!(Elif)],
            vec![T!(_ "elif"), S!(LogicExprL1), S!(Block), S!(Else)]
    );
    rule!(
        NonTerminal::Else =>
            vec![T!(_ "else"), S!(Block)]
    );

    rule!(
        NonTerminal::Loop =>
            vec![T!(_ "loop"), S!(Block)]
    );

    /*
    for r in rules.iter() {
        println!("{:?}:", r.0);
        for rule in r.1 {
            println!("   {}", rule.pretty(None));
        }
    }
    */

    Grammar::new(rules)
}

struct FooLangLexerImpl<'regex, 'input> {
    input: &'input str,
    matches: regex::CaptureMatches<'regex, 'input>,
    escape_re: regex::Regex,
    input_pos: usize,
    lines: Vec<usize>,
}

pub struct FooLangLexerBuilder {
    re: regex::Regex,
}

impl FooLangLexerBuilder {
    pub fn new() -> Self {
        let re = regex::Regex::new(
            r#"(?xm)
        (?<numberhs>-?0x[a-fA-F0-9]+)|
        (?<numberds>0|-?[1-9][0-9]*)|
        (?<byteh>\\x[0-9a-fA-F]{2})|
        (?<byted>\\(?:0|25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9]))|
        (?<ident>@?[a-zA-Z_][a-zA-Z0-9_]*)|
        (?<special>==|<|!=|[(){}\[\].=,;&*/+-])|
        (?<whitespace>\s+)|
        (?<comment>\#.*?$)|
        (?<string>"(?:(:?\\x[0-9a-fA-F]{2})|(:?[[:ascii:]&&[^"\\]])|(:?\\\\)|(:?\\"))+")"#,
        )
        .unwrap();

        Self { re }
    }

    pub fn lex<'this, 'input>(&'this self, input: &'input str) -> impl Lexer + 'this
    where
        'input: 'this,
    {
        FooLangLexerImpl {
            input,
            matches: self.re.captures_iter(input),
            escape_re: regex::Regex::new(r#"(\\x[0-9a-fA-F]{2})|(\\\\|\\")"#).unwrap(),
            input_pos: 0,
            lines: vec![0],
        }
    }
}

impl<'regex, 'input> FooLangLexerImpl<'regex, 'input> {
    fn update_current_line(&mut self, start: usize, processed: &str) {
        for (i, c) in processed.char_indices() {
            if c == '\n' {
                self.lines.push(start + i + 1);
            }
        }
    }
}

impl<'regex, 'input> Lexer for FooLangLexerImpl<'regex, 'input> {
    fn next_token(&mut self, strings: &mut StringPool) -> Result<Token, CompilerError> {
        let ret;

        loop {
            if let Some(m) = self.matches.next() {
                let full = m.get(0).unwrap();
                let lineno = self.lines.len() - 1;
                let range = Some(TokenRange {
                    lineno,
                    begin: full.start(),
                    end: full.end(),
                });
                self.update_current_line(full.start(), full.as_str());

                macro_rules! parse_num {
                    ($t:ty, $radix:expr, $s:expr, $id:tt) => {{
                        let value = <$t>::from_str_radix($s, $radix).map_err(|_| {
                            CompilerError::invalid_number_literal(
                                range.clone().unwrap(),
                                std::mem::size_of::<$t>(),
                            )
                        })?;

                        ret = Token {
                            id: TokenId::$id,
                            meta: value as u64,
                            range,
                        };
                    }};
                }

                if full.start() != self.input_pos {
                    let range = TokenRange {
                        lineno,
                        begin: self.input_pos,
                        end: full.start(),
                    };
                    let invalid = &self.input[range.begin..range.end];
                    return Err(CompilerError::invalid_input_character(range, invalid));
                }
                self.input_pos = full.end();

                if let Some(numberh) = m.name("numberhs") {
                    parse_num!(i64, 16, &numberh.as_str()[2..], LiteralNumber);
                    break;
                } else if let Some(numberd) = m.name("numberds") {
                    parse_num!(i64, 10, numberd.as_str(), LiteralNumber);
                    break;
                } else if let Some(numberh) = m.name("byteh") {
                    parse_num!(u8, 16, &numberh.as_str()[1..], LiteralByte);
                    break;
                } else if let Some(numberd) = m.name("byted") {
                    parse_num!(u8, 10, &numberd.as_str()[1..], LiteralByte);
                    break;
                } else if let Some(ident) = m.name("ident") {
                    let ident = ident.as_str();
                    if let Some(id) = TokenId::from_str(ident) {
                        ret = Token { id, meta: 0, range }
                    } else if ident.starts_with('@') {
                        ret = Token {
                            id: TokenId::BuiltinIdentifier,
                            meta: strings.insert_str(&ident[1..]) as u64,
                            range,
                        };
                    } else {
                        ret = Token {
                            id: TokenId::Identifier,
                            meta: strings.insert_str(ident) as u64,
                            range,
                        };
                    }
                    break;
                } else if let Some(special) = m.name("special") {
                    let id = TokenId::from_str(special.as_str()).unwrap();
                    ret = Token { id, meta: 0, range };
                    break;
                } else if let Some(_whitespace) = m.name("whitespace") {
                    continue;
                } else if let Some(_comment) = m.name("comment") {
                    continue;
                } else if let Some(s) = m.name("string") {
                    let s = s.as_str().trim_start_matches('"').trim_end_matches('"');

                    let mut new = String::with_capacity(s.len());
                    let mut last_match = 0;
                    for caps in self.escape_re.captures_iter(s) {
                        let m = caps.get(0).unwrap();
                        new.push_str(&s[last_match..m.start()]);
                        if let Some(hex_escape) = caps.get(1) {
                            let value = u8::from_str_radix(&hex_escape.as_str()[2..], 16).unwrap();
                            new.push(value as char);
                        } else {
                            let plain_escape = caps.get(2).unwrap();
                            new.push(plain_escape.as_str().chars().nth(1).unwrap());
                        }
                        last_match = m.end();
                    }
                    new.push_str(&s[last_match..]);

                    ret = Token {
                        id: TokenId::LiteralString,
                        meta: strings.insert_str(new.as_str()) as u64,
                        range,
                    };
                    break;
                } else {
                    unreachable!();
                }
            } else {
                return Ok(Token::eof());
            }
        }

        Ok(ret)
    }

    fn get_input_hint(&self, range: &TokenRange, range_b: Option<&TokenRange>) -> String {
        let linestart = self.lines[range.lineno];
        let line = self.input[linestart..].lines().next().unwrap();

        let mut offset = range.begin - linestart;
        let mut size = range.end - range.begin;
        match range_b {
            Some(TokenRange { lineno, begin, end }) if *lineno == range.lineno => {
                let mut offset2 = begin - linestart;
                let mut size2 = end - begin;

                if offset2 < offset {
                    std::mem::swap(&mut offset, &mut offset2);
                    std::mem::swap(&mut size, &mut size2);
                }

                assert!(offset + size <= offset2);
                offset2 -= offset + size;
                format!(
                    "line {}:\n{}\n{:offset$}{:^<size$}{:offset2$}{:^<size2$}",
                    range.lineno, line, "", "", "", ""
                )
            }
            Some(range_b) => {
                let linestart2 = self.lines[range_b.lineno];
                let line2 = self.input[linestart2..].lines().next().unwrap();

                let offset2 = range_b.begin - linestart2;
                let size2 = range_b.end - range_b.begin;

                format!(
                    "line {}:\n{}\n{:offset$}{:^<size$}\nline {}:\n{}\n{:offset2$}{:^<size2$}",
                    range.lineno, line, "", "", range_b.lineno, line2, "", ""
                )
            }
            None => {
                format!(
                    "line {}:\n{}\n{:offset$}{:^<size$}",
                    range.lineno, line, "", ""
                )
            }
        }
    }
}
