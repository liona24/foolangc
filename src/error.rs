use std::collections::BTreeSet;

use crate::codegen::ir::{Function, Name, Type};
use crate::parse_types::{Token, TokenId, TokenRange};
use crate::parsing::Lexer;
use crate::string_pool::StringPool;
use crate::tree_simplification::{ParseTree, ParseTreeNodeRef, ParseTreeNodeType};

pub struct CompilerError {
    range_a: Option<TokenRange>,
    range_b: Option<TokenRange>,
    msg: String,
}

fn find_range_of_subtree(node: &ParseTreeNodeRef) -> Option<TokenRange> {
    let iter = ParseTree::iter_post_order(node.clone());

    let mut lineno = None;
    let mut begin = usize::MAX;
    let mut end = usize::MIN;

    for n in iter {
        if let ParseTreeNodeType::Leaf(t) = &n.borrow().t {
            if let Some(range) = &t.range {
                if *lineno.get_or_insert(range.lineno) == range.lineno {
                    begin = begin.min(range.begin);
                    end = end.max(range.end);
                }
            }
        }
    }

    if let Some(lineno) = lineno {
        Some(TokenRange { lineno, begin, end })
    } else {
        None
    }
}

impl CompilerError {
    pub fn internal_error(msg: &str) -> Self {
        if cfg!(debug_assertions) {
            panic!("{}", msg)
        }

        Self {
            range_a: None,
            range_b: None,
            msg: format!("INTERNAL ERROR: {}", msg),
        }
    }

    pub fn invalid_number_literal(range: TokenRange, size: usize) -> Self {
        Self {
            range_a: Some(range),
            range_b: None,
            msg: format!(
                "invalid number: invalid literal for number of size {}",
                size
            ),
        }
    }

    pub fn invalid_input_character(range: TokenRange, invalid: &str) -> Self {
        Self {
            range_a: Some(range),
            range_b: None,
            msg: format!("invalid input characters: '{}'", invalid),
        }
    }

    pub fn unexpected_token(
        offending_token: Token,
        expected: impl Iterator<Item = TokenId>,
        strings: &StringPool,
    ) -> Self {
        let mut parts = vec![format!(
            "parser error: unexpected token: '{}' expected one of",
            offending_token.pretty(Some(strings))
        )];

        let mut seen = BTreeSet::new();
        for id in expected {
            if seen.insert(id) {
                parts.push(format!("'{}'", id));
            }
        }

        Self {
            range_a: offending_token.range,
            range_b: None,
            msg: parts.join(" "),
        }
    }

    pub fn unexpected_eof() -> Self {
        Self {
            range_a: None,
            range_b: None,
            msg: "parser error: unexpected EOF while parsing input!".to_string(),
        }
    }

    pub fn incompatible_types(
        node_a: ParseTreeNodeRef,
        type_a: &Type,
        node_b: ParseTreeNodeRef,
        type_b: &Type,
        strings: &StringPool,
    ) -> Self {
        let mut range_a = find_range_of_subtree(&node_a);
        let mut range_b = find_range_of_subtree(&node_b);

        if range_a.is_none() {
            range_a = range_b.take();
        }

        Self {
            range_a,
            range_b,
            msg: format!(
                "type error: invalid operation for types '{}' and '{}'\nAre you missing a cast?",
                type_a.pretty(strings),
                type_b.pretty(strings)
            ),
        }
    }

    pub fn invalid_type(
        node: ParseTreeNodeRef,
        node_type: &Type,
        desired_type: &Type,
        strings: &StringPool,
    ) -> Self {
        let range_a = find_range_of_subtree(&node);

        Self {
            range_a,
            range_b: None,
            msg: format!(
                "type error: operation requires type '{}' but type is '{}'\nAre you missing a cast?",
                desired_type.pretty(strings),
                node_type.pretty(strings),
            )
        }
    }

    pub fn deref_non_pointer_type(
        node: ParseTreeNodeRef,
        node_type: &Type,
        strings: &StringPool,
    ) -> Self {
        let range_a = find_range_of_subtree(&node);

        Self {
            range_a,
            range_b: None,
            msg: format!(
                "type error: cannot dereference non-pointer type '{}'",
                node_type.pretty(strings)
            ),
        }
    }

    pub fn unknown_identifier(token: &Token, strings: &StringPool) -> Self {
        let name: Name = token.into();
        Self {
            range_a: token.range.clone(),
            range_b: None,
            msg: format!("unknown identifier: {}", name.pretty(strings)),
        }
    }

    pub fn unknown_function(name: Name, strings: &StringPool) -> Self {
        // TODO: we would want a location for this
        Self {
            range_a: None,
            range_b: None,
            msg: format!("unknown function: {}", name.pretty(strings)),
        }
    }

    pub fn return_outside_function() -> Self {
        // TODO: we would want a location for this
        Self {
            range_a: None,
            range_b: None,
            msg: "structure error: return outside function!".to_string(),
        }
    }

    pub fn invalid_number_of_arguments_for_call(
        args: &[ParseTreeNodeRef],
        func: &Function,
        strings: &StringPool,
    ) -> Self {
        // TODO: we would want a location for this

        if args.len() < func.parameters().len() {
            let mut missing = Vec::new();
            for (name, typ) in func.parameters().iter().skip(args.len()) {
                let typ = typ.upgrade().unwrap();

                missing.push(format!("{}: {}", name.pretty(strings), typ.pretty(strings)));
            }

            Self {
                range_a: None,
                range_b: None,
                msg: format!(
                    "type error: missing arguments {} for function {}",
                    missing.join(", "),
                    func.name().pretty(strings)
                ),
            }
        } else {
            let range_b = find_range_of_subtree(&args[func.parameters().len()]);
            // TODO: only while we do not have range_a
            let range_a = range_b;

            Self {
                range_a,
                range_b: None,
                msg: format!(
                    "type error: too many arguments for function {} which takes {} parameters",
                    func.name().pretty(strings),
                    func.parameters().len()
                ),
            }
        }
    }

    pub fn invalid_member_access(
        base: ParseTreeNodeRef,
        base_type: &Type,
        member: Name,
        strings: &StringPool,
    ) -> Self {
        let range_a = find_range_of_subtree(&base);

        Self {
            range_a,
            range_b: None,
            msg: format!(
                "type error: cannot access member {} on type '{}'",
                member.pretty(strings),
                base_type.pretty(strings)
            ),
        }
    }

    pub fn subscript_on_non_array_type(
        base: ParseTreeNodeRef,
        base_type: &Type,
        strings: &StringPool,
    ) -> Self {
        let range_a = find_range_of_subtree(&base);

        Self {
            range_a,
            range_b: None,
            msg: format!(
                "type error: cannot index non array type '{}'",
                base_type.pretty(strings)
            ),
        }
    }

    pub fn raise(self, lexer: &impl Lexer) -> ! {
        if let Some(range) = self.range_a.as_ref() {
            eprintln!("{}", lexer.get_input_hint(range, self.range_b.as_ref()))
        }
        eprintln!("{}", self.msg);
        std::process::exit(1)
    }
}
