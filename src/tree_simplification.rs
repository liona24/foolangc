use std::cell::RefCell;
use std::collections::BTreeMap;

use std::rc::{Rc, Weak};

use crate::codegen::ir::{Name, Type, TypeCombineResult, TypeContainer, TypeId, VReg};
use crate::error::CompilerError;
use crate::parse_types::{NonTerminal, Symbol, Token, TokenId};
use crate::parsing::{NodeLabel, ParseForest};
use crate::string_pool::StringPool;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    BitAnd,
    BitOr,
    Eq,
    Neq,
    Lt,
}

impl BinaryOp {
    pub fn mnemonic(&self, signed: bool) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => {
                if signed {
                    "imul"
                } else {
                    "mul"
                }
            }
            Self::BitAnd => "and",
            Self::BitOr => "or",
            Self::Eq | Self::Neq | Self::Lt => unreachable!(),
        }
    }
}

impl From<&Token> for BinaryOp {
    fn from(value: &Token) -> Self {
        match value.id {
            TokenId::Plus => Self::Add,
            TokenId::Minus => Self::Sub,
            TokenId::Star => Self::Mul,
            TokenId::KeyAnd => Self::BitAnd,
            TokenId::KeyOr => Self::BitOr,
            TokenId::Eq => Self::Eq,
            TokenId::Neq => Self::Neq,
            TokenId::Lt => Self::Lt,
            _ => {
                println!("{:?}", value);
                unreachable!();
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
    AddrOf,
}

impl From<&Token> for UnaryOp {
    fn from(value: &Token) -> Self {
        match value.id {
            TokenId::KeyNot => Self::Not,
            TokenId::Minus => Self::Neg,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum OffsetValue {
    Member(Name),
    Value(ParseTreeNodeRef),
}

#[derive(Debug)]
pub enum ParseTreeNodeType {
    Statements(Vec<ParseTreeNodeRef>),
    DeclVar {
        typ: ParseTreeNodeRef,
        name: Name,
    },
    Assignment {
        target: ParseTreeNodeRef,
        value: ParseTreeNodeRef,
    },
    BinaryExpression {
        left: ParseTreeNodeRef,
        right: ParseTreeNodeRef,
        op: BinaryOp,
    },
    UnaryExpression {
        operand: ParseTreeNodeRef,
        op: UnaryOp,
    },
    /// This is a special node only used for type propagation.
    /// It basically is the same thing as a Deref, but it will be ignored during code generation
    LValue {
        reference: ParseTreeNodeRef,
    },
    Deref {
        reference: ParseTreeNodeRef,
    },
    Leaf(Token),
    Type(TypeId),
    Struct {
        name: Name,
        fields: Vec<(Name, ParseTreeNodeRef)>,
    },
    Func {
        typ: Option<ParseTreeNodeRef>,
        name: Name,
        past_end_label: Name,
        params: Vec<(Name, ParseTreeNodeRef)>,
        block: ParseTreeNodeRef,
    },
    FuncEnd {
        past_end_label: Name,
        func: ParseTreeNodeRef,
    },
    Call {
        func: Name,
        args: ParseTreeNodeRef,
    },
    CallArgs {
        func: Name,
        args: Vec<ParseTreeNodeRef>,
    },
    Return {
        value: ParseTreeNodeRef,
    },
    Arg {
        value: ParseTreeNodeRef,
    },
    Cast {
        value: ParseTreeNodeRef,
        dst: Option<ParseTreeNodeRef>,
    },
    Offset {
        of: ParseTreeNodeRef,
        value: OffsetValue,
    },
    Loop {
        body: ParseTreeNodeRef,
        name: Name,
    },
    LoopEnd {
        name: Name,
        lop: ParseTreeNodeRef,
    },
    Block {
        name: Name,
        body: ParseTreeNodeRef,
    },
    Marker {
        body: ParseTreeNodeRef,
        label: Name,
        continuation: Option<Name>,
    },
    If {
        condition: ParseTreeNodeRef,
        true_label: Name,
        true_block: ParseTreeNodeRef,
        false_label: Name,
        false_block: ParseTreeNodeRef,
    },
    Nop {
        next: ParseTreeNodeRef,
    },
    Empty,
}

#[derive(Debug)]
pub struct ParseTreeNode {
    pub result: VReg,
    pub typ: Option<Rc<Type>>,
    pub t: ParseTreeNodeType,
    parent: Weak<RefCell<ParseTreeNode>>,
}

pub type ParseTreeNodeRef = Rc<RefCell<ParseTreeNode>>;

impl ParseTreeNode {
    fn empty_ref(parent: Weak<RefCell<ParseTreeNode>>) -> ParseTreeNodeRef {
        Rc::new(RefCell::new(Self {
            result: VReg::next(),
            typ: None,
            t: ParseTreeNodeType::Empty,
            parent,
        }))
    }

    fn new(
        parent: Weak<RefCell<ParseTreeNode>>,
        t: ParseTreeNodeType,
        typ: Option<Rc<Type>>,
    ) -> ParseTreeNode {
        let mut res = Self {
            result: VReg::next(),
            typ: None,
            t,
            parent,
        };

        if let Some(typ) = typ {
            res.add_type_info(typ)
        }

        res
    }

    fn add_type_info(&mut self, t: Rc<Type>) {
        assert!(self.typ.is_none());

        let size = if t.is_ref() {
            t.deref_ptr_or_ref().unwrap().size()
        } else {
            t.size()
        };
        self.result.add_info(size, t.is_ref(), t.is_signed());

        self.typ = Some(t)
    }

    pub fn typ(&self) -> Rc<Type> {
        self.typ.as_ref().unwrap().clone()
    }
}

pub struct ParseTree {
    pub root: ParseTreeNodeRef,
    pub types: TypeContainer,
}

struct ParseTreePostOrderIter {
    stack: Vec<ParseTreeNodeRef>,
    node: Option<ParseTreeNodeRef>,
    prev_node: Option<ParseTreeNodeRef>,
}

impl Iterator for ParseTreePostOrderIter {
    type Item = ParseTreeNodeRef;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(n) = self.node.take() {
                self.stack.push(n.clone());

                match &n.borrow().t {
                    ParseTreeNodeType::BinaryExpression { right, .. }
                    | ParseTreeNodeType::UnaryExpression { operand: right, .. }
                    | ParseTreeNodeType::Assignment { value: right, .. }
                    | ParseTreeNodeType::FuncEnd { func: right, .. }
                    | ParseTreeNodeType::LoopEnd { lop: right, .. }
                    | ParseTreeNodeType::Marker { body: right, .. }
                    | ParseTreeNodeType::Deref { reference: right }
                    | ParseTreeNodeType::LValue { reference: right }
                    | ParseTreeNodeType::Call { args: right, .. }
                    | ParseTreeNodeType::Cast { value: right, .. }
                    | ParseTreeNodeType::Arg { value: right }
                    | ParseTreeNodeType::Return { value: right }
                    | ParseTreeNodeType::Nop { next: right, .. }
                    | ParseTreeNodeType::If {
                        condition: right, ..
                    } => self.node = Some(right.clone()),
                    ParseTreeNodeType::Offset { value, of } => {
                        if let OffsetValue::Value(right) = value {
                            self.node = Some(right.clone())
                        } else {
                            self.node = Some(of.clone())
                        }
                    }
                    ParseTreeNodeType::Statements(_)
                    | ParseTreeNodeType::CallArgs { .. }
                    | ParseTreeNodeType::Leaf(_)
                    | ParseTreeNodeType::Empty
                    | ParseTreeNodeType::DeclVar { .. }
                    | ParseTreeNodeType::Type(_)
                    | ParseTreeNodeType::Func { .. }
                    | ParseTreeNodeType::Loop { .. }
                    | ParseTreeNodeType::Block { .. }
                    | ParseTreeNodeType::Struct { .. } => {}
                }
            } else if let Some(top) = self.stack.last() {
                let top = top.clone();
                match &top.borrow().t {
                    ParseTreeNodeType::Statements(_)
                    | ParseTreeNodeType::Leaf(_)
                    | ParseTreeNodeType::DeclVar { .. }
                    | ParseTreeNodeType::UnaryExpression { .. }
                    | ParseTreeNodeType::Func { .. }
                    | ParseTreeNodeType::FuncEnd { .. }
                    | ParseTreeNodeType::LoopEnd { .. }
                    | ParseTreeNodeType::Marker { .. }
                    | ParseTreeNodeType::Block { .. }
                    | ParseTreeNodeType::Call { .. }
                    | ParseTreeNodeType::CallArgs { .. }
                    | ParseTreeNodeType::Arg { .. }
                    | ParseTreeNodeType::Struct { .. }
                    | ParseTreeNodeType::Nop { .. }
                    | ParseTreeNodeType::Return { .. }
                    | ParseTreeNodeType::Loop { .. }
                    | ParseTreeNodeType::If { .. }
                    | ParseTreeNodeType::LValue { .. }
                    | ParseTreeNodeType::Deref { .. }
                    | ParseTreeNodeType::Cast { dst: None, .. } => {
                        self.prev_node = self.stack.pop();
                        break self.prev_node.clone();
                    }
                    ParseTreeNodeType::Type(_) | ParseTreeNodeType::Empty => {
                        self.prev_node = self.stack.pop();
                    }
                    ParseTreeNodeType::BinaryExpression { left, .. }
                    | ParseTreeNodeType::Assignment { target: left, .. }
                    | ParseTreeNodeType::Offset { of: left, .. }
                    | ParseTreeNodeType::Cast {
                        dst: Some(left), ..
                    } => {
                        if self
                            .prev_node
                            .as_ref()
                            .map_or(false, |n| !Rc::ptr_eq(n, left))
                        {
                            self.node = Some(left.clone());
                        } else {
                            self.prev_node = self.stack.pop();
                            break self.prev_node.clone();
                        }
                    }
                };
            } else {
                break None;
            }
        }
    }
}

impl ParseTree {
    pub fn iter_post_order(root: ParseTreeNodeRef) -> impl Iterator<Item = ParseTreeNodeRef> {
        ParseTreePostOrderIter {
            stack: Vec::new(),
            node: Some(root),
            prev_node: None,
        }
    }

    pub fn new(forest: &ParseForest, strings: &mut StringPool) -> Result<Self, CompilerError> {
        let new_root = ParseTreeNode::empty_ref(Weak::default());
        let mut types = TypeContainer::new(strings);

        let mut s = vec![(&forest.root, new_root.clone())];
        let empty = vec![];

        while let Some((n, implant)) = s.pop() {
            macro_rules! family {
                ($x:expr) => {{
                    let families = forest.families.get($x).unwrap_or(&empty);
                    assert!(families.len() == 1);
                    &families[0]
                }};
            }
            macro_rules! assume_token {
                ($node:expr) => {
                    if let NodeLabel::Symbol(Symbol::Terminal { token }) = &$node.label {
                        token
                    } else {
                        return Err(CompilerError::internal_error("unexpected parse tree!"));
                    }
                };
            }
            macro_rules! is_nonterminal {
                ($node:expr, $nonterm:tt) => {{
                    if let NodeLabel::Symbol(Symbol::NonTerminal(NonTerminal::$nonterm)) =
                        &$node.label
                    {
                        true
                    } else {
                        false
                    }
                }};
            }
            macro_rules! propagate {
                () => {
                    let f = family!(n);
                    s.push((&f.a, implant));
                    assert!(f.b.is_none());
                };
                ($parent:expr) => {
                    let f = family!(n);
                    s.push((&f.a, $parent));
                    assert!(f.b.is_none());
                };
            }

            macro_rules! new_child {
                () => {
                    ParseTreeNode::empty_ref(Rc::downgrade(&implant))
                };
                ($parent:expr) => {
                    ParseTreeNode::empty_ref(Rc::downgrade($parent))
                };
            }

            macro_rules! new_block {
                ($parent:expr, $label:expr, $continuation:expr) => {{
                    let end = new_child!($parent);
                    let block = new_child!(&end);
                    let body = new_child!(&block);

                    end.borrow_mut().t = ParseTreeNodeType::Marker {
                        body: block.clone(),
                        label: $label.end(),
                        continuation: $continuation,
                    };

                    block.borrow_mut().t = ParseTreeNodeType::Block {
                        name: $label,
                        body: body.clone(),
                    };

                    (end, body)
                }};
            }

            match &n.label {
                NodeLabel::Symbol(Symbol::NonTerminal(nt)) => match nt {
                    NonTerminal::Start => {
                        propagate!();
                    }
                    NonTerminal::Block => {
                        let f = family!(n);
                        if let NodeLabel::Item(_) = &f.a.label {
                            let f = family!(&f.a);
                            s.push((f.b.as_ref().unwrap(), implant));
                        }
                    }
                    NonTerminal::Statement | NonTerminal::GlobalStatement => {
                        let f = family!(n);
                        if let Some(b) = f.b.as_ref() {
                            assert!(assume_token!(b).id == TokenId::Stop);
                        }
                        s.push((&f.a, implant));
                    }
                    NonTerminal::Statements | NonTerminal::GlobalStatements => {
                        let mut statements = vec![];

                        let mut tmp = n;
                        loop {
                            let f = family!(tmp);

                            let new = new_child!();
                            statements.push(new.clone());

                            if let Some(b) = f.b.as_ref() {
                                tmp = &f.a;

                                s.push((b, new));
                            } else {
                                s.push((&f.a, new));
                                break;
                            }
                        }

                        implant.borrow_mut().t = ParseTreeNodeType::Statements(statements);
                    }
                    NonTerminal::Return => {
                        let f = family!(n);
                        let value = new_child!();

                        if let Some(b) = f.b.as_ref() {
                            s.push((b, value.clone()));
                        }

                        implant.borrow_mut().t = ParseTreeNodeType::Return { value };
                    }
                    NonTerminal::Decl => {
                        let f = family!(n);
                        let b = f.b.as_ref().unwrap();

                        let value = new_child!();
                        s.push((b, value.clone()));

                        let next = family!(&f.a);
                        assert!(assume_token!(next.b.as_ref().unwrap()).id == TokenId::Assign);

                        let next_next = family!(&next.a);

                        let target = new_child!();
                        let reference = new_child!(&target);

                        let typ = new_child!(&reference);
                        s.push((&next_next.a, typ.clone()));

                        let name = assume_token!(next_next.b.as_ref().unwrap()).into();

                        reference.borrow_mut().t = ParseTreeNodeType::DeclVar { typ, name };
                        target.borrow_mut().t = ParseTreeNodeType::LValue { reference };
                        implant.borrow_mut().t = ParseTreeNodeType::Assignment { target, value };
                    }
                    NonTerminal::Assign => {
                        let f = family!(n);
                        let b = f.b.as_ref().unwrap();

                        let value = new_child!();
                        s.push((b, value.clone()));

                        let next = family!(&f.a);
                        assert!(assume_token!(next.b.as_ref().unwrap()).id == TokenId::Assign);

                        let target = new_child!();
                        s.push((&next.a, target.clone()));

                        implant.borrow_mut().t = ParseTreeNodeType::Assignment { target, value };
                    }
                    NonTerminal::Func => {
                        let f = family!(n);

                        let func = new_child!();

                        let block = new_child!(&func);
                        s.push((f.b.as_ref().unwrap(), block.clone()));

                        let mut return_type = None;
                        let mut params = Vec::new();
                        let name;

                        let mut tmp = &f.a;
                        loop {
                            let f = family!(tmp);
                            let b = f.b.as_ref().unwrap();

                            match &b.label {
                                NodeLabel::Symbol(Symbol::NonTerminal(NonTerminal::Type)) => {
                                    let typ = new_child!(&func);
                                    s.push((b, typ.clone()));
                                    return_type = Some(typ);
                                }
                                NodeLabel::Symbol(Symbol::NonTerminal(NonTerminal::Params)) => {
                                    let mut tmp = b;
                                    let mut stack: Vec<Name> = Vec::new();
                                    loop {
                                        let f = family!(tmp);
                                        let b = f.b.as_ref().unwrap();

                                        macro_rules! add_param {
                                            ($type_node:expr) => {{
                                                let name = stack.pop().unwrap();
                                                let t = new_child!(&func);

                                                s.push(($type_node, t.clone()));
                                                params.push((name, t));
                                            }};
                                        }

                                        match &b.label {
                                            NodeLabel::Symbol(Symbol::NonTerminal(
                                                NonTerminal::Type,
                                            )) => {
                                                add_param!(&b);
                                            }
                                            NodeLabel::Symbol(Symbol::Terminal { token }) => {
                                                if token.id == TokenId::Identifier {
                                                    stack.push(token.into());
                                                }
                                            }
                                            _ => unreachable!(),
                                        }

                                        if is_nonterminal!(&f.a, Type) {
                                            add_param!(&f.a);
                                            break;
                                        } else {
                                            tmp = &f.a;
                                        }
                                    }
                                }
                                NodeLabel::Symbol(Symbol::Terminal { token })
                                    if token.id == TokenId::Identifier =>
                                {
                                    assert!(assume_token!(f.a).id == TokenId::KeyFunc);

                                    name = token.into();
                                    break;
                                }
                                _ => {}
                            }

                            tmp = &f.a;
                        }

                        let past_end_label = Name::new_internal();

                        func.borrow_mut().t = ParseTreeNodeType::Func {
                            name,
                            past_end_label,
                            typ: return_type,
                            params,
                            block,
                        };

                        implant.borrow_mut().t = ParseTreeNodeType::FuncEnd {
                            past_end_label,
                            func,
                        };
                    }
                    NonTerminal::Params => todo!(),
                    NonTerminal::Struct => {
                        let f = family!(n);

                        let left = family!(&f.a);
                        let left_left = family!(&left.a);
                        let left_left_left = family!(&left_left.a);
                        let name: Name = assume_token!(left_left_left.b.as_ref().unwrap()).into();

                        let mut fields = Vec::new();
                        macro_rules! add_field {
                            ($node:expr) => {
                                assert!(is_nonterminal!($node, StructField));

                                let typ = new_child!();
                                let f = family!($node);
                                let next = family!(&f.a);

                                let name = assume_token!(next.b.as_ref().unwrap()).into();
                                s.push((&next.a, typ.clone()));

                                fields.push((name, typ));
                            };
                        }

                        let mut fields_node = family!(left.b.as_ref().unwrap());
                        loop {
                            if let Some(b) = fields_node.b.as_ref() {
                                add_field!(b);
                                fields_node = family!(&fields_node.a);
                            } else {
                                add_field!(&fields_node.a);
                                break;
                            }
                        }

                        types.hint_future_type(name);
                        implant.borrow_mut().t = ParseTreeNodeType::Struct { name, fields };
                    }
                    NonTerminal::StructFields | NonTerminal::StructField => unreachable!(),
                    NonTerminal::Call => {
                        let mut f = family!(n);
                        let mut args = Vec::new();

                        let call = new_child!();
                        let call_args = new_child!(&call);

                        loop {
                            if let NodeLabel::Symbol(Symbol::Terminal { token }) = &f.a.label {
                                let func = token.into();

                                implant.borrow_mut().t = ParseTreeNodeType::Call {
                                    args: call_args.clone(),
                                    func,
                                };
                                call_args.borrow_mut().t =
                                    ParseTreeNodeType::CallArgs { func, args };
                                break;
                            }

                            let b = f.b.as_ref().unwrap();
                            if is_nonterminal!(b, Args) {
                                let mut tmp = b;

                                loop {
                                    let f = family!(tmp);

                                    if let Some(b) = &f.b {
                                        let arg = new_child!(&call_args);
                                        s.push((b, arg.clone()));
                                        args.push(arg);

                                        let next = family!(&f.a);
                                        tmp = &next.a;
                                    } else {
                                        let arg = new_child!(&call_args);
                                        s.push((&f.a, arg.clone()));
                                        args.push(arg);

                                        break;
                                    }
                                }
                            }

                            f = family!(&f.a);
                        }
                    }
                    NonTerminal::Arg => {
                        let value = new_child!();
                        propagate!(value.clone());

                        implant.borrow_mut().t = ParseTreeNodeType::Arg { value };
                    }
                    NonTerminal::LogicExprL1
                    | NonTerminal::LogicExprL2
                    | NonTerminal::ExprL1
                    | NonTerminal::ExprL2 => {
                        let f = family!(n);
                        if let Some(b) = f.b.as_ref() {
                            let left = new_child!();
                            let right = new_child!();

                            let next_left = family!(&f.a);
                            let op_token = assume_token!(next_left.b.as_ref().unwrap());

                            s.push((&next_left.a, left.clone()));
                            s.push((b, right.clone()));

                            implant.borrow_mut().t = ParseTreeNodeType::BinaryExpression {
                                left,
                                right,
                                op: op_token.into(),
                            }
                        } else {
                            s.push((&f.a, implant));
                        }
                    }
                    NonTerminal::LogicExprL3 => {
                        let f = family!(n);
                        let b = f.b.as_ref().unwrap();

                        if is_nonterminal!(b, ExprL1) {
                            let next = family!(&f.a);
                            let op_token = assume_token!(next.b.as_ref().unwrap());

                            let left = new_child!();
                            let right = new_child!();

                            s.push((&next.a, left.clone()));
                            s.push((b, right.clone()));

                            implant.borrow_mut().t = ParseTreeNodeType::BinaryExpression {
                                left,
                                right,
                                op: op_token.into(),
                            }
                        } else {
                            let next = family!(&f.a);
                            let operand = new_child!();
                            s.push((next.b.as_ref().unwrap(), operand.clone()));

                            implant.borrow_mut().t = ParseTreeNodeType::UnaryExpression {
                                operand,
                                op: UnaryOp::Not,
                            };
                        }
                    }
                    NonTerminal::ExprL3 => {
                        let f = family!(n);

                        if let Some(b) = f.b.as_ref() {
                            if is_nonterminal!(b, ExprL3) {
                                let op = assume_token!(&f.a);

                                let operand = new_child!();
                                s.push((b, operand.clone()));

                                implant.borrow_mut().t = ParseTreeNodeType::UnaryExpression {
                                    operand,
                                    op: op.into(),
                                };
                            } else {
                                let next = family!(&f.a);
                                s.push((next.b.as_ref().unwrap(), implant));
                            }
                        } else {
                            s.push((&f.a, implant));
                        }
                    }
                    NonTerminal::Type => {
                        enum Variant {
                            Array { count: u32 },
                            Pointer,
                        }

                        let mut stack = Vec::with_capacity(2);
                        let base;

                        let mut tmp = n;
                        loop {
                            let f = family!(tmp);

                            match &f.a.label {
                                NodeLabel::Symbol(Symbol::NonTerminal(NonTerminal::Array)) => {
                                    let next = family!(&f.a);
                                    let next_next = family!(&next.a);

                                    let count = assume_token!(next_next.b.as_ref().unwrap());
                                    assert!(count.id == TokenId::LiteralNumber);
                                    let count = count.meta as u32;

                                    stack.push(Variant::Array { count });

                                    let next_next_next = family!(&next_next.a);
                                    tmp = &next_next_next.a;
                                }
                                NodeLabel::Symbol(Symbol::NonTerminal(NonTerminal::Pointer)) => {
                                    stack.push(Variant::Pointer);

                                    let next = family!(&f.a);
                                    assert!(
                                        assume_token!(next.b.as_ref().unwrap()).id == TokenId::Star
                                    );
                                    tmp = &next.a;
                                }
                                NodeLabel::Symbol(Symbol::Terminal { token })
                                    if token.id == TokenId::KeyStruct =>
                                {
                                    let ident = assume_token!(f.b.as_ref().unwrap());
                                    assert!(ident.id == TokenId::Identifier);
                                    base = TypeId::UserDefined { name: ident.into() };
                                    break;
                                }
                                NodeLabel::Symbol(Symbol::Terminal { token })
                                    if token.id == TokenId::KeyInt =>
                                {
                                    base = TypeId::BuiltinS64;
                                    break;
                                }
                                NodeLabel::Symbol(Symbol::Terminal { token })
                                    if token.id == TokenId::KeyByte =>
                                {
                                    base = TypeId::BuiltinU8;
                                    break;
                                }
                                _ => unreachable!(),
                            }
                        }

                        let mut id = base;
                        for v in stack.into_iter().rev() {
                            match v {
                                Variant::Array { count } => {
                                    id = types.insert_array(id, count).id;
                                }
                                Variant::Pointer => {
                                    id = types.insert_pointer(id).id;
                                }
                            }
                        }

                        implant.borrow_mut().t = ParseTreeNodeType::Type(id);
                    }
                    NonTerminal::Literal => {
                        let f = family!(n);

                        let t = assume_token!(&f.a);
                        implant.borrow_mut().t = ParseTreeNodeType::Leaf(t.clone());
                    }
                    NonTerminal::Ref => {
                        let f = family!(n);

                        if let NodeLabel::Symbol(Symbol::Terminal { token }) = &f.a.label {
                            implant.borrow_mut().t = ParseTreeNodeType::Leaf(token.clone());
                        } else {
                            let b = assume_token!(f.b.as_ref().unwrap());
                            if b.id == TokenId::ClosingSquare {
                                // array index
                                let next = family!(&f.a);

                                let value = new_child!();
                                s.push((next.b.as_ref().unwrap(), value.clone()));

                                let next_next = family!(&next.a);
                                let of = new_child!();
                                s.push((&next_next.a, of.clone()));

                                implant.borrow_mut().t = ParseTreeNodeType::Offset {
                                    value: OffsetValue::Value(value),
                                    of,
                                };
                            } else {
                                let name: Name = b.into();

                                let next = family!(&f.a);
                                assert!(assume_token!(next.b.as_ref().unwrap()).id == TokenId::Dot);

                                let of = new_child!();
                                s.push((&next.a, of.clone()));

                                implant.borrow_mut().t = ParseTreeNodeType::Offset {
                                    of,
                                    value: OffsetValue::Member(name),
                                };
                            }
                        }
                    }
                    NonTerminal::Deref => {
                        let f = family!(n);
                        let b = f.b.as_ref().unwrap();

                        let reference = new_child!();
                        assert!(assume_token!(&f.a).id == TokenId::Star);
                        s.push((b, reference.clone()));
                        implant.borrow_mut().t = ParseTreeNodeType::Deref { reference };
                    }
                    NonTerminal::AddrOf => {
                        let f = family!(n);

                        let operand = new_child!();
                        s.push((f.b.as_ref().unwrap(), operand.clone()));

                        implant.borrow_mut().t = ParseTreeNodeType::UnaryExpression {
                            operand,
                            op: UnaryOp::AddrOf,
                        };
                    }
                    NonTerminal::Rhs => {
                        let reference = new_child!();
                        propagate!(reference.clone());
                        implant.borrow_mut().t = ParseTreeNodeType::Deref { reference };
                    }
                    NonTerminal::Lhs => {
                        let reference = new_child!();
                        propagate!(reference.clone());
                        implant.borrow_mut().t = ParseTreeNodeType::LValue { reference };
                    }
                    NonTerminal::If | NonTerminal::Elif => {
                        let f = family!(n);
                        let b = f.b.as_ref().unwrap();

                        let end = new_child!();
                        let if_ = new_child!(&end);
                        let continuation_label = Name::new_internal();

                        let true_label = Name::new_internal();
                        let (true_block, true_) =
                            new_block!(&if_, true_label, Some(continuation_label));

                        let false_label = Name::new_internal();
                        let (false_block, false_) =
                            new_block!(&if_, false_label, Some(continuation_label));

                        let condition = new_child!(&if_);

                        let has_else_branch = !is_nonterminal!(b, Block);
                        if has_else_branch {
                            s.push((b, false_.clone()));

                            let next = family!(&f.a);
                            s.push((next.b.as_ref().unwrap(), true_.clone()));

                            let next_next = family!(&next.a);
                            s.push((next_next.b.as_ref().unwrap(), condition.clone()));
                        } else {
                            s.push((b, true_.clone()));

                            let next = family!(&f.a);
                            s.push((next.b.as_ref().unwrap(), condition.clone()));
                        }

                        if_.borrow_mut().t = ParseTreeNodeType::If {
                            condition,
                            true_label,
                            true_block,
                            false_label,
                            false_block,
                        };
                        implant.borrow_mut().t = ParseTreeNodeType::Marker {
                            body: if_,
                            label: continuation_label,
                            continuation: None,
                        };
                    }
                    NonTerminal::Else => {
                        let f = family!(n);
                        assert!(assume_token!(f.a).id == TokenId::KeyElse);
                        s.push((f.b.as_ref().unwrap(), implant));
                    }
                    NonTerminal::Loop => {
                        let f = family!(n);

                        let lop = new_child!();

                        let body = new_child!(&lop);
                        s.push((f.b.as_ref().unwrap(), body.clone()));

                        let name = Name::new_internal();
                        lop.borrow_mut().t = ParseTreeNodeType::Loop { body, name };

                        implant.borrow_mut().t = ParseTreeNodeType::LoopEnd { name, lop };
                    }
                    NonTerminal::Array | NonTerminal::Pointer | NonTerminal::Args => {
                        unreachable!()
                    }
                },
                NodeLabel::Symbol(Symbol::Terminal { token }) => {
                    implant.borrow_mut().t = ParseTreeNodeType::Leaf(token.clone());
                }
                NodeLabel::Item(_) => {}
            }
        }

        add_type_information(new_root.clone(), &mut types, strings)?;

        Ok(ParseTree {
            root: new_root,
            types,
        })
    }

    #[cfg(feature = "graphviz-rust")]
    #[allow(unused)]
    pub fn dump(&self, strings: &StringPool) -> String {
        use graphviz_rust::cmd::Format;
        use graphviz_rust::dot_generator::{attr, edge, graph, id, node, node_id, stmt};
        use graphviz_rust::dot_structures::*;
        use graphviz_rust::exec;
        use graphviz_rust::printer::PrinterContext;

        let mut g = graph!(strict di id!("parsetree"));

        let get_node_id = |n: &ParseTreeNodeRef, i: usize| {
            let id = Rc::as_ptr(n) as usize;

            format!("n{}_{}", id, i)
        };

        let new_node = |id: &str, n: &ParseTreeNodeRef| {
            let label = match &n.borrow().t {
                ParseTreeNodeType::Statements(_) => "statements".to_string(),
                ParseTreeNodeType::BinaryExpression { op, .. } => format!("{:?}", op),
                ParseTreeNodeType::UnaryExpression { op, .. } => format!("{:?}", op),
                ParseTreeNodeType::Leaf(t) => {
                    format!("\"{}\"", t.pretty(Some(strings)))
                }
                ParseTreeNodeType::Empty => "\"\"".to_string(),
                ParseTreeNodeType::DeclVar { name, .. } => {
                    format!("\"var {}\"", name.to_label(strings))
                }
                ParseTreeNodeType::Type(t) => format!("type {:?}", t),
                ParseTreeNodeType::Assignment { .. } => "=".to_string(),
                ParseTreeNodeType::Func { name, .. } => {
                    format!("func {}", name.to_label(strings))
                }
                ParseTreeNodeType::FuncEnd {
                    past_end_label: label,
                    ..
                } => {
                    format!("end of func {}", label.to_label(strings))
                }
                ParseTreeNodeType::LoopEnd { name, .. } => {
                    format!("end of loop {}", name.to_label(strings))
                }
                ParseTreeNodeType::Block { name, .. } => {
                    format!("block {}", name.to_label(strings))
                }
                ParseTreeNodeType::Marker { label, .. } => {
                    format!("marker {}", label.to_label(strings))
                }
                ParseTreeNodeType::Call { .. } => "call".to_string(),
                ParseTreeNodeType::CallArgs { func, .. } => {
                    format!("call args {}", func.to_label(strings))
                }
                ParseTreeNodeType::Arg { .. } => "arg".to_string(),
                ParseTreeNodeType::Return { .. } => "return".to_string(),
                ParseTreeNodeType::Loop { .. } => "loop".to_string(),
                ParseTreeNodeType::Deref { .. } => "deref".to_string(),
                ParseTreeNodeType::LValue { .. } => "lvalue".to_string(),
                ParseTreeNodeType::Cast { .. } => "cast".to_string(),
                ParseTreeNodeType::Struct { name, .. } => {
                    format!("struct {}", name.to_label(strings))
                }
                ParseTreeNodeType::If { .. } => "if".to_string(),
                ParseTreeNodeType::Offset { .. } => "offset".to_string(),
                ParseTreeNodeType::Nop { .. } => "nop".to_string(),
            };

            let label = format!(
                "{}\n{}",
                label,
                n.borrow()
                    .typ
                    .as_ref()
                    .map_or("<empty>".to_string(), |t| format!("{:?}", t))
            );
            let label = format!("\"{}\"", label.escape_debug());

            node!(id;
                attr!("style","\"filled,rounded\""),
                attr!("fillcolor","\"#fafaf0\""),
                attr!("shape","box"),
                attr!("margin","\"0.5,0.25\""),
                attr!("label",label)
            )
        };

        let mut s: Vec<ParseTreeNodeRef> = vec![self.root.clone()];
        let mut visited = std::collections::BTreeSet::new();
        while let Some(n) = s.pop() {
            let id = get_node_id(&n, 0);
            g.add_stmt(stmt!(new_node(&id, &n)));
            if let Some(parent) = n.borrow().parent.upgrade() {
                let parent_id = get_node_id(&parent, 0);
                g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&parent_id))));
            }

            assert!(visited.insert(Rc::as_ptr(&n) as usize));

            match &n.borrow().t {
                ParseTreeNodeType::CallArgs { args: stmts, .. }
                | ParseTreeNodeType::Statements(stmts) => {
                    for child in stmts.iter() {
                        let child_id = get_node_id(child, 0);
                        g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));

                        s.push(child.clone());
                    }
                }
                ParseTreeNodeType::DeclVar { typ: n, .. }
                | ParseTreeNodeType::FuncEnd { func: n, .. }
                | ParseTreeNodeType::LoopEnd { lop: n, .. }
                | ParseTreeNodeType::UnaryExpression { operand: n, .. }
                | ParseTreeNodeType::Arg { value: n }
                | ParseTreeNodeType::Block { body: n, .. }
                | ParseTreeNodeType::Marker { body: n, .. }
                | ParseTreeNodeType::Cast {
                    value: n,
                    dst: None,
                }
                | ParseTreeNodeType::Return { value: n }
                | ParseTreeNodeType::Call { args: n, .. }
                | ParseTreeNodeType::Nop { next: n, .. }
                | ParseTreeNodeType::Loop { body: n, .. }
                | ParseTreeNodeType::Deref { reference: n }
                | ParseTreeNodeType::LValue { reference: n } => {
                    let child_id = get_node_id(n, 0);
                    g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));

                    s.push(n.clone());
                }
                ParseTreeNodeType::BinaryExpression { left, right, .. }
                | ParseTreeNodeType::Assignment {
                    target: left,
                    value: right,
                }
                | ParseTreeNodeType::Cast {
                    value: right,
                    dst: Some(left),
                } => {
                    let child_id = get_node_id(left, 0);
                    g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));
                    s.push(left.clone());

                    let child_id = get_node_id(right, 0);
                    g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));
                    s.push(right.clone());
                }
                ParseTreeNodeType::Offset {
                    of: left,
                    value: right,
                } => {
                    let child_id = get_node_id(left, 0);
                    g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));
                    s.push(left.clone());

                    if let OffsetValue::Value(right) = right {
                        let child_id = get_node_id(right, 0);
                        g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));
                        s.push(right.clone());
                    }
                }
                ParseTreeNodeType::Struct { fields, .. } => {
                    for (name, child) in fields.iter() {
                        let label = format!("\"{}\"", name.to_label(strings).escape_debug());

                        let child_id = get_node_id(child, 0);
                        g.add_stmt(stmt!(
                            edge!(node_id!(&id) => node_id!(&child_id); attr!("label", label))
                        ));

                        s.push(child.clone());
                    }
                }
                ParseTreeNodeType::Func {
                    typ, params, block, ..
                } => {
                    if let Some(typ) = typ {
                        let child_id = get_node_id(typ, 0);
                        g.add_stmt(stmt!(
                            edge!(node_id!(&id) => node_id!(&child_id); attr!("label", "type"))
                        ));
                        s.push(typ.clone());
                    }

                    for (name, p) in params.iter() {
                        let child_id = get_node_id(p, 0);
                        let label = format!("\"{}\"", name.to_label(strings).escape_debug());
                        g.add_stmt(stmt!(
                            edge!(node_id!(&id) => node_id!(&child_id); attr!("label", label))
                        ));
                        s.push(p.clone());
                    }

                    let child_id = get_node_id(block, 0);
                    g.add_stmt(stmt!(edge!(node_id!(&id) => node_id!(&child_id))));
                    s.push(block.clone());
                }
                ParseTreeNodeType::If {
                    condition,
                    true_block,
                    false_block,
                    ..
                } => {
                    let child_id = get_node_id(condition, 0);
                    g.add_stmt(stmt!(
                        edge!(node_id!(&id) => node_id!(&child_id); attr!("label", "condition"))
                    ));
                    s.push(condition.clone());

                    let child_id = get_node_id(true_block, 0);
                    g.add_stmt(stmt!(
                        edge!(node_id!(&id) => node_id!(&child_id); attr!("label", "true"))
                    ));
                    s.push(true_block.clone());

                    let child_id = get_node_id(false_block, 0);
                    g.add_stmt(stmt!(
                        edge!(node_id!(&id) => node_id!(&child_id); attr!("label", "false"))
                    ));
                    s.push(false_block.clone());
                }
                ParseTreeNodeType::Type(_)
                | ParseTreeNodeType::Leaf(_)
                | ParseTreeNodeType::Empty => {}
            }
        }

        exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()]).unwrap()
    }
}

struct VariableContainer {
    vars: Vec<(Name, BTreeMap<Name, Rc<Type>>)>,
}

impl VariableContainer {
    fn new() -> Self {
        Self {
            vars: vec![(Name::Internal(0), BTreeMap::new())],
        }
    }

    fn add(&mut self, name: Name, typ: Rc<Type>) {
        self.vars.last_mut().unwrap().1.insert(name, typ);
    }

    fn get(&self, name: Name) -> Option<Rc<Type>> {
        for (_, vars) in self.vars.iter().rev() {
            if let Some(t) = vars.get(&name) {
                return Some(t.clone());
            }
        }

        None
    }

    fn current_function(&self) -> Option<Name> {
        if self.vars.len() > 1 {
            Some(self.vars.last().unwrap().0)
        } else {
            None
        }
    }

    fn enter_function(&mut self, name: Name) {
        self.vars.push((name, BTreeMap::new()));
    }

    fn leave_function(&mut self) {
        self.vars.pop().unwrap();
    }
}

fn add_type_information(
    root: ParseTreeNodeRef,
    types: &mut TypeContainer,
    strings: &StringPool,
) -> Result<(), CompilerError> {
    // General idea: First we let type information flow from bottom to the top.
    // Then we fill in missing pieces by doing a top down traversal and check whether everything works
    // out.

    let mut variables = VariableContainer::new();

    macro_rules! get_type_of_typenode {
        ($node:expr) => {{
            if let ParseTreeNodeType::Type(typ) = $node.borrow().t {
                types.get_type(typ).unwrap()
            } else {
                unreachable!();
            }
        }};
    }

    macro_rules! type_of {
        ($node:expr) => {
            $node.borrow().typ.as_ref().unwrap().clone()
        };
    }

    let mut top_down = Vec::new();

    let void = types.get_type(TypeId::Void).unwrap();

    let mut stack = vec![ParseTree::iter_post_order(root.clone())];
    loop {
        if stack.is_empty() {
            break;
        }

        let iter = stack.last_mut().unwrap();

        if let Some(nn) = iter.next() {
            top_down.push(nn.clone());

            macro_rules! insert_cast {
                ($node:expr, $target_type:expr) => {
                    let replacement = ParseTreeNode::empty_ref($node.borrow().parent.clone());
                    $node.borrow_mut().parent = Rc::downgrade(&replacement);

                    replacement.borrow_mut().add_type_info($target_type);
                    replacement.borrow_mut().t = ParseTreeNodeType::Cast {
                        value: $node.clone(),
                        dst: None,
                    };

                    *$node = replacement;
                };
            }

            macro_rules! try_combine_types {
                ($node_a:expr, $node_b:expr) => {{
                    let type_a = type_of!($node_a);
                    let type_b = type_of!($node_b);

                    println!("try_combine_types:");
                    println!("  type a: {:?}", type_a);
                    println!("  type b: {:?}", type_b);

                    match Type::combine_types(&type_a, &type_b) {
                        TypeCombineResult::Ok(res) => res,
                        TypeCombineResult::CastA(res) => {
                            insert_cast!($node_a, res.clone());
                            res
                        }
                        TypeCombineResult::CastB(res) => {
                            insert_cast!($node_b, res.clone());
                            res
                        }
                        TypeCombineResult::Err => {
                            return Err(CompilerError::incompatible_types(
                                $node_a.clone(),
                                &type_a,
                                $node_b.clone(),
                                &type_b,
                                strings,
                            ))
                        }
                    }
                }};
            }

            macro_rules! check_type_compatible {
                ($node:expr, $target_type:expr) => {
                    let typ = type_of!($node);

                    println!("check_type_compatible:");
                    println!("  desired: {:?}", $target_type);
                    println!("  sub: {:?}", typ);

                    match Type::combine_types(&$target_type, &typ) {
                        TypeCombineResult::Ok(_) => {}
                        TypeCombineResult::CastB(res) => {
                            insert_cast!($node, res);
                        }
                        TypeCombineResult::CastA(_) | TypeCombineResult::Err => {
                            return Err(CompilerError::invalid_type(
                                $node.clone(),
                                &typ,
                                &$target_type,
                                strings,
                            ));
                        }
                    }
                };
            }

            let mut n = nn.borrow_mut();
            match &mut n.t {
                ParseTreeNodeType::Statements(stmts) => {
                    for stmt in stmts.iter() {
                        stack.push(ParseTree::iter_post_order(stmt.clone()));
                    }
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::DeclVar { typ, name } => {
                    let t = types.insert_ref(get_type_of_typenode!(typ).id);
                    variables.add(*name, t.clone());

                    n.add_type_info(t);
                }
                ParseTreeNodeType::UnaryExpression { operand, op } => match op {
                    UnaryOp::Not => {
                        let target_type = types.get_type(TypeId::BuiltinU8).unwrap();
                        check_type_compatible!(operand, target_type);

                        n.add_type_info(target_type);
                    }
                    UnaryOp::Neg => {
                        let target_type = types.get_type(TypeId::BuiltinS64).unwrap();
                        check_type_compatible!(operand, target_type);

                        n.add_type_info(target_type);
                    }
                    UnaryOp::AddrOf => {
                        let t = type_of!(operand);
                        assert!(t.is_ref());

                        let target_type = types.insert_pointer(t.as_deref().id);
                        n.add_type_info(target_type);
                    }
                },
                ParseTreeNodeType::BinaryExpression { left, right, op } => match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::BitAnd
                    | BinaryOp::BitOr => {
                        let result_type = try_combine_types!(left, right);

                        if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul) {
                            eprintln!("TODO: check that result_type is number");
                        }

                        n.add_type_info(result_type);
                    }
                    BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt => {
                        eprintln!(
                            "TODO: check that left / right are numbers with good sign for lt"
                        );
                        let _ = try_combine_types!(left, right);

                        let target_type = types.get_type(TypeId::BuiltinU8).unwrap();
                        n.add_type_info(target_type);
                    }
                },
                ParseTreeNodeType::Assignment { target, value } => {
                    let target_type = type_of!(target);
                    let value_type = type_of!(value);

                    println!("check assignment:");
                    println!("   left: {:?}", target_type);
                    println!("  right: {:?}", value_type);

                    match target_type.assign(value_type.as_ref()) {
                        TypeCombineResult::Ok(_) => {}
                        TypeCombineResult::CastA(_) => unreachable!(),
                        TypeCombineResult::CastB(_res) => {
                            // TODO: in theory this should be the complex version of the cast with
                            // dst set. but we simply hope that this is handled implicitly by the code generator
                            // right now.
                        }
                        TypeCombineResult::Err => {
                            return Err(CompilerError::incompatible_types(
                                target.clone(),
                                &target_type,
                                value.clone(),
                                &value_type,
                                strings,
                            ))
                        }
                    }

                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Deref { reference } => {
                    let typ = type_of!(reference);

                    if typ.is_value_ref() {
                        n.add_type_info(typ.as_deref());
                    } else {
                        if typ.deref_ptr_or_ref().is_none() {
                            return Err(CompilerError::deref_non_pointer_type(
                                reference.clone(),
                                &typ,
                                strings,
                            ));
                        }

                        n.t = ParseTreeNodeType::Nop {
                            next: reference.clone(),
                        };
                        n.add_type_info(typ);
                    }
                }
                ParseTreeNodeType::LValue { reference } => {
                    let mut typ = type_of!(reference);

                    if let Some(pointee) = typ.as_pointer() {
                        typ = types.insert_ref(pointee.id);
                    } else if !typ.is_ref() {
                        return Err(CompilerError::deref_non_pointer_type(
                            reference.clone(),
                            &typ,
                            strings,
                        ));
                    }

                    n.add_type_info(typ);
                }
                ParseTreeNodeType::Leaf(token) => match token.id {
                    TokenId::Identifier => {
                        let name = token.as_ref().into();
                        let t = variables
                            .get(name)
                            .ok_or_else(|| CompilerError::unknown_identifier(token, strings))?;
                        n.add_type_info(t);
                    }
                    TokenId::LiteralByte => {
                        let t = types.get_type(TypeId::BuiltinU8).unwrap();
                        n.add_type_info(t);
                    }
                    TokenId::LiteralNumber => {
                        let t = types.get_type(TypeId::BuiltinS64).unwrap();
                        n.add_type_info(t);
                    }
                    TokenId::LiteralString => {
                        let s = strings.get(token.meta as u32);
                        let count = s.as_bytes().len() as u32;

                        let t = types.insert_array(TypeId::BuiltinU8, count);
                        let t = types.insert_ref(t.id);
                        n.add_type_info(t);
                    }
                    TokenId::KeyBreak | TokenId::KeyContinue => {
                        eprintln!("TODO: verify that we are in a loop");
                        n.add_type_info(void.clone());
                    }
                    _ => unreachable!(),
                },
                ParseTreeNodeType::Struct { name, fields } => {
                    let fields: Vec<_> = fields
                        .iter()
                        .map(|(name, type_node)| (*name, get_type_of_typenode!(type_node).id))
                        .collect();

                    types.insert_blob(*name, fields);
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Type(_) => unreachable!(),
                ParseTreeNodeType::Func {
                    typ,
                    name,
                    params,
                    block,
                    ..
                } => {
                    let return_type = if let Some(typ) = typ {
                        get_type_of_typenode!(typ).id
                    } else {
                        TypeId::Void
                    };

                    variables.enter_function(*name);

                    stack.push(ParseTree::iter_post_order(block.clone()));

                    let parameters: Vec<_> = params
                        .iter()
                        .map(|(name, type_node)| {
                            let t = get_type_of_typenode!(type_node);
                            variables.add(*name, types.insert_ref(t.id).clone());

                            (*name, t.id)
                        })
                        .collect();

                    types.insert_function(*name, parameters.into_iter(), return_type);
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::FuncEnd { .. } => {
                    n.add_type_info(void.clone());
                    variables.leave_function();
                }
                ParseTreeNodeType::Call { func, args } => {
                    let f = types
                        .get_function(*func)
                        .ok_or_else(|| CompilerError::unknown_function(*func, strings))?;

                    if let ParseTreeNodeType::CallArgs { args, .. } = &mut args.borrow_mut().t {
                        if args.len() != f.parameters().len() {
                            return Err(CompilerError::invalid_number_of_arguments_for_call(
                                args, &f, strings,
                            ));
                        }

                        for (arg_provider, (_, target_type)) in args.iter().zip(f.parameters()) {
                            let target_type = target_type.upgrade().unwrap();

                            if let ParseTreeNodeType::Arg { value } =
                                &mut arg_provider.borrow_mut().t
                            {
                                check_type_compatible!(value, target_type);
                            } else {
                                unreachable!();
                            }
                        }
                    } else {
                        unreachable!();
                    };

                    n.add_type_info(f.return_type().clone());
                }
                ParseTreeNodeType::CallArgs { func, args } => {
                    let f = types
                        .get_function(*func)
                        .ok_or_else(|| CompilerError::unknown_function(*func, strings))?;

                    for arg in args.iter() {
                        stack.push(ParseTree::iter_post_order(arg.clone()));
                    }

                    n.add_type_info(f.return_type().clone());
                }
                ParseTreeNodeType::Return { value } => {
                    let f = variables
                        .current_function()
                        .ok_or_else(|| CompilerError::return_outside_function())?;
                    let f = types.get_function(f).unwrap();
                    let target_type = f.return_type();
                    check_type_compatible!(value, target_type);

                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Offset { value, of } => {
                    let base_type = type_of!(of);
                    assert!(base_type.is_ref());
                    let base_type = base_type.as_deref();

                    let result_type;

                    match value {
                        OffsetValue::Member(name) => {
                            let (offset, result_type_weak) = base_type
                                .as_blob()
                                .map(|blob| blob.members().get(name).cloned())
                                .flatten()
                                .ok_or_else(|| {
                                    CompilerError::invalid_member_access(
                                        of.clone(),
                                        &base_type,
                                        *name,
                                        strings,
                                    )
                                })?;

                            let replacement = Rc::new(RefCell::new(ParseTreeNode::new(
                                Rc::downgrade(&nn),
                                ParseTreeNodeType::Leaf(Token {
                                    id: TokenId::LiteralNumber,
                                    meta: offset as u64,
                                    range: None, // TODO: not sure if this may fail. If it can, we should include the proper position here.
                                }),
                                types.get_type(TypeId::BuiltinS64),
                            )));

                            result_type = types.insert_ref(result_type_weak.upgrade().unwrap().id);
                            *value = OffsetValue::Value(replacement);
                        }
                        OffsetValue::Value(value) => {
                            let element_type_id = base_type
                                .as_array()
                                .map(|array| array.element_type().id)
                                .ok_or_else(|| {
                                    CompilerError::subscript_on_non_array_type(
                                        of.clone(),
                                        &base_type,
                                        strings,
                                    )
                                })?;

                            let target_type = types.get_type(TypeId::BuiltinS64).unwrap();
                            check_type_compatible!(value, target_type);

                            result_type = types.insert_ref(element_type_id);
                        }
                    }

                    n.add_type_info(result_type);
                }
                ParseTreeNodeType::If {
                    condition,
                    true_block,
                    false_block,
                    ..
                } => {
                    let target_type = types.get_type(TypeId::BuiltinU8).unwrap();
                    check_type_compatible!(condition, target_type);

                    stack.push(ParseTree::iter_post_order(true_block.clone()));
                    stack.push(ParseTree::iter_post_order(false_block.clone()));

                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Nop { next } => {
                    let t = type_of!(next);
                    n.add_type_info(t);
                }
                ParseTreeNodeType::Arg { .. } => {
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Marker { .. } => {
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Loop { body, .. } | ParseTreeNodeType::Block { body, .. } => {
                    stack.push(ParseTree::iter_post_order(body.clone()));
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::LoopEnd { name: _, lop: _ } => {
                    n.add_type_info(void.clone());
                }
                ParseTreeNodeType::Cast { .. } | ParseTreeNodeType::Empty => unreachable!(),
            }

            assert!(n.typ.is_some());
        } else {
            stack.pop();
        }
    }

    Ok(())
}
