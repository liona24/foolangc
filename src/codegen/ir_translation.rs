use std::collections::BTreeMap;
use std::rc::Rc;

use super::allocator::VariableAllocator;
use super::ir::{
    Address, Function, Ir, IrOp, IrStatement, Name, Type, TypeContainer, TypeId, VReg, Variable,
};
use crate::parse_types::TokenId;
use crate::string_pool::StringPool;
use crate::tree_simplification::{ParseTree, ParseTreeNodeType};

struct Scope {
    variables: BTreeMap<Name, Variable>,
    allocator: VariableAllocator,
}

struct GlobalData<'g> {
    ir: Vec<IrStatement<'g>>,
    scopes: Vec<Scope>,
    loops: Vec<Name>,
    types: &'g TypeContainer,
    void: Rc<Type>,
    constants: BTreeMap<u32, Name>,
}

impl<'g> GlobalData<'g> {
    fn new(types: &'g TypeContainer, _strings: &mut StringPool) -> Self {
        let allocator = VariableAllocator::new_global();

        let void = types.get_type(TypeId::Void).unwrap();

        let ans = Self {
            ir: vec![IrStatement::new(
                VReg::discard(),
                void.clone(),
                IrOp::EnterScopePlaceholder,
            )],
            scopes: vec![Scope {
                variables: BTreeMap::new(),
                allocator,
            }],
            loops: Vec::new(),
            types,
            void,
            constants: BTreeMap::new(),
        };

        ans
    }

    fn add_constant(&mut self, id: u32) -> Address {
        Address::Global {
            name: *self
                .constants
                .entry(id)
                .or_insert_with(|| Name::new_internal()),
        }
    }

    fn add_discard_ir_op(&mut self, op: IrOp) {
        self.ir
            .push(IrStatement::new(VReg::discard(), self.void.clone(), op));
    }
    fn add_ir_op(&mut self, result: VReg, typ: Rc<Type>, op: IrOp) {
        self.ir.push(IrStatement::new(result, typ, op));
    }

    fn finalize(mut self) -> Ir<'g> {
        assert!(self.scopes.len() == 1);
        self.exit_scope();

        let mut defs = BTreeMap::new();

        let mut old_ir = Vec::with_capacity(self.ir.len() * 2);
        std::mem::swap(&mut old_ir, &mut self.ir);

        for (i, stmt) in old_ir.into_iter().enumerate() {
            match &stmt.op {
                IrOp::LoadConst { .. }
                | IrOp::LoadAddr { .. }
                | IrOp::EnterScope(_)
                | IrOp::LeaveScope(_)
                | IrOp::Label(_)
                | IrOp::JmpLabel { .. }
                | IrOp::Call { .. } => {}

                IrOp::Binary { left, right, .. } => {
                    assert!(defs.remove(left).is_some());
                    assert!(defs.remove(right).is_some());
                }
                IrOp::MemAssign { src, dst } => {
                    assert!(defs.remove(src).is_some());
                    assert!(defs.remove(dst).is_some());
                }
                IrOp::MemLoad { mem_src: reg }
                | IrOp::Return { value: reg, .. }
                | IrOp::Unary { operand: reg, .. }
                | IrOp::BranchCond { cond: reg, .. }
                | IrOp::Extend { operand: reg, .. }
                | IrOp::ProvideArg(reg)
                | IrOp::Nop(reg) => {
                    assert!(defs.remove(reg).is_some());
                }

                IrOp::None | IrOp::Kill(_) | IrOp::EnterScopePlaceholder => unreachable!(),
            }

            // we basically blow up the whole ir with IrOp::Nones for each existing op.
            // then we will replace some of them IrOp::Kill() if the result of the previous IrOp is
            // to be discarded.
            // Not optimal, but better than all the copying..
            if stmt.result != VReg::discard() {
                defs.insert(stmt.result, i * 2 + 1);
            }
            self.ir.push(stmt);
            self.ir.push(IrStatement::new(
                VReg::discard(),
                self.void.clone(),
                IrOp::None,
            ));
        }

        for (vreg, i) in defs {
            self.ir[i].op = IrOp::Kill(vreg);
        }

        self.ir
            .retain(|stmt| std::mem::discriminant(&stmt.op) != std::mem::discriminant(&IrOp::None));

        // TODO: what else?

        Ir {
            ir: self.ir,
            constants: self.constants,
        }
    }

    fn find_variable(&self, name: Name) -> Option<&Variable> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables.get(&name) {
                return Some(var);
            }
        }

        None
    }

    fn add_variable(&mut self, name: Name, typ: TypeId) -> Address {
        if let Some(typ) = self.types.get_type(typ) {
            let scope = self.scopes.last_mut().unwrap();
            if !scope.variables.contains_key(&name) {
                let addr = scope.allocator.allocate(name, typ.size());
                let var = Variable { addr, typ };

                scope.variables.insert(name, var);
                addr
            } else {
                unreachable!("invalid input: variable shadow. type checker failed");
            }
        } else {
            unreachable!("invalid input: type not found. type checker failed");
        }
    }

    fn add_param(&mut self, name: Name, typ: Rc<Type>) -> Address {
        let scope = self.scopes.last_mut().unwrap();
        let addr = scope.allocator.add_param(typ.size());
        let var = Variable { addr, typ };

        if scope.variables.insert(name, var).is_some() {
            eprintln!("warning: variable shadows other variable in same scope!");
        }

        addr
    }

    fn find_function(&mut self, name: Name) -> Option<Function> {
        self.types
            .get_type(TypeId::UserDefined { name })
            .map(|x| x.as_function())
            .flatten()
    }

    fn enter_function(&mut self, name: Name) {
        let f = self.find_function(name).unwrap();

        self.scopes.push(Scope {
            variables: BTreeMap::new(),
            allocator: VariableAllocator::new_local(f.name()),
        });

        self.ir.push(IrStatement::new(
            VReg::discard(),
            self.void.clone(),
            IrOp::EnterScopePlaceholder,
        ));

        for (name, typ) in f.parameters() {
            self.add_param(*name, typ.upgrade().unwrap());
        }
    }

    fn current_ret_label(&self) -> Name {
        self.scopes.last().unwrap().allocator.name.end()
    }

    fn enter_loop(&mut self, name: Name) {
        self.add_ir_op(VReg::discard(), self.void.clone(), IrOp::Label(name));
        self.loops.push(name);
    }

    fn exit_loop(&mut self, name: Name) {
        self.add_ir_op(VReg::discard(), self.void.clone(), IrOp::JmpLabel { name });
        self.add_ir_op(VReg::discard(), self.void.clone(), IrOp::Label(name.end()));
        assert!(name == self.loops.pop().unwrap());
    }

    fn current_loop(&self) -> Name {
        *self.loops.last().unwrap()
    }

    fn reserve_spill(&mut self) {
        self.scopes.last_mut().unwrap().allocator.reserve_spill();
    }

    fn exit_scope(&mut self) {
        assert!(self.scopes.len() > 0);
        let scope = self.scopes.pop().unwrap();
        let name = scope.allocator.name;

        for stmt in self.ir.iter_mut().rev() {
            if std::mem::discriminant(&stmt.op)
                == std::mem::discriminant(&IrOp::EnterScopePlaceholder)
            {
                stmt.op = IrOp::EnterScope(scope.allocator);
                break;
            }
        }

        self.ir.push(IrStatement::new(
            VReg::discard(),
            self.void.clone(),
            IrOp::LeaveScope(name.end()),
        ));
    }
}

pub fn translate<'t>(tree: &'t ParseTree, strings: &mut StringPool) -> Ir<'t> {
    let mut g = GlobalData::new(&tree.types, strings);

    macro_rules! as_type {
        ($node:expr) => {{
            if let ParseTreeNodeType::Type(typ) = $node.borrow().t {
                typ
            } else {
                unreachable!();
            }
        }};
    }

    let mut stack = vec![ParseTree::iter_post_order(tree.root.clone())];
    loop {
        if stack.is_empty() {
            break;
        }

        let iter = stack.last_mut().unwrap();

        if let Some(n) = iter.next() {
            let n = n.borrow();
            match &n.t {
                ParseTreeNodeType::Statements(stmts) => {
                    for stmt in stmts.iter() {
                        stack.push(ParseTree::iter_post_order(stmt.clone()));
                    }
                }
                ParseTreeNodeType::Func {
                    typ: _,
                    name,
                    past_end_label,
                    params: _,
                    block,
                } => {
                    g.add_discard_ir_op(IrOp::JmpLabel {
                        name: *past_end_label,
                    });

                    g.enter_function(*name);

                    stack.push(ParseTree::iter_post_order(block.clone()));
                }
                ParseTreeNodeType::FuncEnd { past_end_label, .. } => {
                    g.exit_scope();
                    g.add_discard_ir_op(IrOp::Label(*past_end_label));
                }
                ParseTreeNodeType::Loop { body, name } => {
                    g.enter_loop(*name);
                    stack.push(ParseTree::iter_post_order(body.clone()));
                }
                ParseTreeNodeType::LoopEnd { name, .. } => {
                    g.exit_loop(*name);
                }
                ParseTreeNodeType::Block { name, body } => {
                    g.add_discard_ir_op(IrOp::Label(*name));
                    stack.push(ParseTree::iter_post_order(body.clone()));
                }
                ParseTreeNodeType::Marker {
                    label,
                    continuation,
                    ..
                } => {
                    g.add_discard_ir_op(IrOp::Label(*label));
                    if let Some(continuation) = continuation {
                        g.add_discard_ir_op(IrOp::JmpLabel {
                            name: *continuation,
                        });
                    }
                }
                ParseTreeNodeType::CallArgs { args, .. } => {
                    for arg in args.iter() {
                        stack.push(ParseTree::iter_post_order(arg.clone()));
                    }
                }
                ParseTreeNodeType::Call { func, .. } => {
                    let func = *func;
                    g.reserve_spill();
                    let arg_size = g.find_function(func).unwrap().arg_size();

                    g.add_ir_op(n.result, n.typ(), IrOp::Call { func, arg_size });
                }
                ParseTreeNodeType::Return { value } => {
                    g.add_discard_ir_op(IrOp::Return {
                        value: value.borrow().result, // TODO: value might be empty
                        to: g.current_ret_label(),
                    });
                }
                ParseTreeNodeType::Arg { value } => {
                    g.add_discard_ir_op(IrOp::ProvideArg(value.borrow().result));
                }
                ParseTreeNodeType::DeclVar { typ, name } => {
                    let typ = as_type!(typ);
                    let addr = g.add_variable(*name, typ);
                    g.add_ir_op(n.result, n.typ(), IrOp::LoadAddr { addr });
                }
                ParseTreeNodeType::BinaryExpression {
                    op, left, right, ..
                } => {
                    let left = left.borrow().result;
                    let right = right.borrow().result;

                    g.add_ir_op(
                        n.result,
                        n.typ(),
                        IrOp::Binary {
                            op: *op,
                            left,
                            right,
                        },
                    );
                }
                ParseTreeNodeType::UnaryExpression { operand, op } => g.add_ir_op(
                    n.result,
                    n.typ(),
                    IrOp::Unary {
                        operand: operand.borrow().result,
                        op: *op,
                    },
                ),
                ParseTreeNodeType::Assignment { target, value } => {
                    assert!(value.borrow().typ().is_ref() == value.borrow().result.is_ref());

                    g.add_discard_ir_op(IrOp::MemAssign {
                        src: value.borrow().result,
                        dst: target.borrow().result,
                    });
                }
                ParseTreeNodeType::Offset { of, value } => {
                    if let crate::tree_simplification::OffsetValue::Value(value) = value {
                        g.add_ir_op(
                            n.result,
                            n.typ(),
                            IrOp::Binary {
                                op: crate::tree_simplification::BinaryOp::Add,
                                left: of.borrow().result.ref_to_ptr(),
                                right: value.borrow().result,
                            },
                        );
                    } else {
                        unreachable!()
                    }
                }
                ParseTreeNodeType::Cast { value, dst } => {
                    assert!(dst.is_none(), "not implemented");
                    assert!(n.typ().is_value_type());

                    g.add_ir_op(
                        n.result,
                        n.typ(),
                        IrOp::Extend {
                            operand: value.borrow().result,
                        },
                    );
                }
                ParseTreeNodeType::LValue { reference: next } | ParseTreeNodeType::Nop { next } => {
                    g.add_ir_op(n.result, n.typ(), IrOp::Nop(next.borrow().result));
                }
                ParseTreeNodeType::Deref { reference } => {
                    g.add_ir_op(
                        n.result,
                        n.typ(),
                        IrOp::MemLoad {
                            mem_src: reference.borrow().result,
                        },
                    );
                }
                ParseTreeNodeType::If {
                    condition,
                    true_label,
                    true_block,
                    false_label,
                    false_block,
                } => {
                    g.add_discard_ir_op(IrOp::BranchCond {
                        true_branch: *true_label,
                        false_branch: *false_label,
                        cond: condition.borrow().result,
                    });

                    stack.push(ParseTree::iter_post_order(false_block.clone()));
                    stack.push(ParseTree::iter_post_order(true_block.clone()));
                }
                ParseTreeNodeType::Leaf(t) => match t.id {
                    TokenId::Identifier | TokenId::BuiltinIdentifier => {
                        if let Some(var) = g.find_variable(t.into()) {
                            g.add_ir_op(n.result, n.typ(), IrOp::LoadAddr { addr: var.addr });
                        } else {
                            todo!("invalid input: unknown reference");
                        }
                    }
                    TokenId::LiteralNumber | TokenId::LiteralByte => {
                        g.add_ir_op(n.result, n.typ(), IrOp::LoadConst { value: t.meta });
                    }
                    TokenId::LiteralString => {
                        let addr = g.add_constant(t.meta as u32);
                        g.add_ir_op(n.result, n.typ(), IrOp::LoadAddr { addr });
                    }
                    TokenId::KeyBreak => {
                        let lop = g.current_loop();
                        g.add_discard_ir_op(IrOp::JmpLabel { name: lop.end() });
                    }
                    TokenId::KeyContinue => {
                        let lop = g.current_loop();
                        g.add_discard_ir_op(IrOp::JmpLabel { name: lop });
                    }
                    _ => unreachable!(),
                },
                ParseTreeNodeType::Struct { .. } => {}
                ParseTreeNodeType::Empty | ParseTreeNodeType::Type(_) => unreachable!(),
            }
        } else {
            stack.pop();
        }
    }

    g.finalize()
}
