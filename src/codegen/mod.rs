pub mod allocator;
pub mod ir;
pub mod ir_translation;
mod register_state;

use crate::string_pool::StringPool;
use crate::tree_simplification::{BinaryOp, UnaryOp};
use allocator::VariableAllocatorImpl;
use ir::{Address, IrOp};
use register_state::{Reg, RegisterState};

use self::ir::Ir;

fn load_addr(
    mut out: impl std::io::Write,
    addr: Address,
    dst: Reg,
    strings: &StringPool,
) -> Result<(), std::io::Error> {
    match addr {
        Address::Global { name } => {
            write!(
                out,
                "    lea {}(%rip), {}\n",
                name.to_label(strings),
                dst.n8()
            )?;
        }
        Address::Local(offset) => {
            write!(out, "    lea -{}(%rbp), {}\n", offset, dst.n8())?;
        }
        Address::Argument(offset) => {
            write!(
                out,
                "    lea {}(%rbp), {}\n",
                offset + 16, /*saved rbp + return address*/
                dst.n8()
            )?;
        }
    }

    Ok(())
}

fn deref_small_blob(
    mut out: impl std::io::Write,
    in_out_reg: Reg,
    tmp: Reg,
    size: u32,
) -> Result<(), std::io::Error> {
    assert!(size <= 8);
    if size == 8 {
        write!(out, "    movq ({}), {}\n", in_out_reg.n8(), in_out_reg.n8())?;
        return Ok(());
    }

    write!(out, "    pushq {}\n", tmp.n8())?;
    write!(out, "    xorq {}, {}\n", tmp.n8(), tmp.n8(),)?;
    write!(out, "    pushq {}\n", tmp.n8())?;

    for i in 0..size {
        write!(out, "    movb {}({}), {}\n", i, in_out_reg.n8(), tmp.n1())?;
        write!(out, "    movb {}, {}(%rsp)\n", tmp.n1(), i)?;
    }

    write!(out, "    popq {}\n", in_out_reg.n8())?;
    write!(out, "    popq {}\n", tmp.n8())?;

    Ok(())
}

pub fn generate(
    mut out: impl std::io::Write,
    ir: &Ir<'_>,
    strings: &StringPool,
) -> Result<(), std::io::Error> {
    let mut stack: Vec<(Option<Address>, RegisterState)> = Vec::new();
    let mut globals = Vec::new();

    macro_rules! regs {
        () => {
            &mut stack.last_mut().unwrap().1
        };
    }
    macro_rules! spill_addr {
        () => {
            stack.last().unwrap().0.unwrap()
        };
    }
    macro_rules! alloc_reg {
        ($vreg:expr, $requires:expr) => {
            regs!().alloc_reg(&mut out, $vreg, $requires)
        };
    }
    macro_rules! reuse_reg {
        ($vreg:expr, $new:expr, $requires:expr) => {
            regs!().reuse_reg(&mut out, $vreg, $new, $requires)
        };
    }
    macro_rules! use_reg {
        ($vreg:expr, $requires:expr) => {
            regs!().use_reg(&mut out, $vreg, $requires)
        };
    }
    macro_rules! alloc_tmp_reg {
        ($requires:expr) => {
            regs!().alloc_tmp_reg(&mut out, $requires)
        };
    }

    macro_rules! emit {
            ($s:expr $(,$arg:expr)*) => {
                write!(&mut out, concat!($s, "\n") $(,$arg)*)?
            }
        }

    emit!(".section .text\n");
    emit!(".globl _start");
    emit!("_start:");
    emit!("    lea .Ldo_exit(%rip), %rax");
    emit!("    pushq %rax");

    for stmt in ir.ir.iter() {
        emit!("    # {:?} of {:?} = {:?}", stmt.result, stmt.typ, stmt.op);

        match stmt.op {
            IrOp::None => {}
            IrOp::Kill(reg) => {
                regs!().kill_vreg(reg);
            }
            IrOp::LoadConst { value } => {
                let dst = alloc_reg!(stmt.result, Reg::any())?;
                emit!("    mov $0x{:x}, {}", value, dst.n(stmt.result.size()));
            }
            IrOp::Unary { op, operand } => {
                let size = stmt.typ.size();

                match op {
                    UnaryOp::Not => {
                        // We have to be a bit careful here, because most of the time we are lazy and truthness
                        // bits will be scattered.
                        let reg = reuse_reg!(operand, stmt.result, Reg::any())?;
                        emit!("    and {}, {}", reg.n(size), reg.n(size));
                        emit!("    sete {}", reg.n(size));
                    }
                    UnaryOp::Neg => {
                        let reg = reuse_reg!(operand, stmt.result, Reg::any())?;
                        emit!("    neg {}", reg.n(size));
                    }
                    UnaryOp::AddrOf => {
                        assert!(operand.is_ref());
                        reuse_reg!(operand, stmt.result, Reg::any())?;
                    }
                }
            }
            IrOp::Binary {
                op,
                mut left,
                mut right,
            } => {
                let size = stmt.typ.size();

                match op {
                    BinaryOp::Mul => {
                        assert!(left.is_value());
                        assert!(right.is_value());
                        assert!(left.size() == right.size());

                        let _dst = reuse_reg!(left, stmt.result, Reg::rax())?;
                        let _clobber = alloc_tmp_reg!(Reg::rdx())?;
                        let arg = use_reg!(right, Reg::any())?;

                        if stmt.result.is_signed() {
                            emit!("    imul {}", arg.n(size));
                        } else {
                            emit!("    mul {}", arg.n(size));
                        }
                    }
                    BinaryOp::Neq | BinaryOp::Eq => {
                        let e = if op == BinaryOp::Eq { "e" } else { "ne" };

                        if left.is_value() && right.is_value() {
                            assert!(left.size() == right.size());
                            let a = use_reg!(left, Reg::any())?;
                            let b = use_reg!(right, Reg::any())?;
                            let res = alloc_reg!(stmt.result, Reg::any())?;

                            emit!("    xor {}, {}", a.n(left.size()), b.n(left.size()));
                            emit!("    set{} {}", e, res.n(size));
                        } else if left.is_ref() && right.is_ref() {
                            if left.deref_size() < right.deref_size() {
                                std::mem::swap(&mut left, &mut right);
                            }

                            let _a = use_reg!(left, Reg::rdi())?;
                            let _b = use_reg!(right, Reg::rsi())?;
                            let tmp = alloc_tmp_reg!(Reg::rax())?;
                            let res = alloc_reg!(stmt.result, Reg::rcx())?;

                            emit!("    mov $0x{:x}, {}", right.deref_size(), res.n8());
                            emit!("    repe cmpsb");

                            let bytes_left = left.deref_size() - right.deref_size();
                            if bytes_left == 0 {
                                emit!("    set{} {}", e, res.n(size));
                            } else {
                                emit!("    j{} 1f", e);
                                emit!("    xorb {}, {}", tmp.n1(), tmp.n1());
                                emit!("    mov $0x{:x}, {}", bytes_left, res.n8());
                                emit!("    repe scasb");
                                emit!("1:");
                                emit!("    set{} {}", e, res.n(size));
                            }
                        } else {
                            if right.is_ref() {
                                std::mem::swap(&mut left, &mut right);
                            }

                            let a = use_reg!(left, Reg::rdi())?;
                            let tmp = alloc_tmp_reg!(Reg::rax())?;
                            let res = alloc_reg!(stmt.result, Reg::rcx())?;
                            let b = use_reg!(right, Reg::any())?;

                            if right.size() < 8 {
                                emit!("    movzx {}, {}", b.n(right.size()), b.n8());
                            }

                            let mut bytes_left = left.deref_size();

                            let used = bytes_left.min(8);

                            let tmp2 = res;
                            emit!("    movq {}, {}", a.n8(), tmp2.n8());
                            deref_small_blob(&mut out, tmp2, tmp, used)?;
                            emit!("    xorq {}, {}", tmp2.n8(), b.n8());
                            bytes_left -= used;
                            if bytes_left == 0 {
                                emit!("    set{} {}", e, res.n(size));
                            } else {
                                emit!("    j{} 1f", e);
                                emit!("    addq ${}, {}", used, a.n8());
                                emit!("    xorb {}, {}", tmp.n1(), tmp.n1());
                                emit!("    mov $0x{:x}, {}", bytes_left, res.n8());
                                emit!("    repe scasb");
                                emit!("1:");
                                emit!("    set{} {}", e, res.n(size));
                            }
                        }
                    }
                    BinaryOp::Lt => {
                        assert!(left.is_value());
                        assert!(right.is_value());
                        assert!(left.size() == right.size());

                        let res = alloc_reg!(stmt.result, Reg::any())?;
                        let a = use_reg!(left, Reg::any())?;
                        let b = use_reg!(right, Reg::any())?;

                        emit!("    sub {}, {}", b.n(left.size()), a.n(left.size()));
                        if left.is_signed() {
                            emit!("    setl {}", res.n(size));
                        } else {
                            emit!("    setb {}", res.n(size));
                        }
                    }
                    _ => {
                        assert!(left.is_value());
                        assert!(right.is_value());
                        assert!(left.size() == right.size());

                        let dst = reuse_reg!(left, stmt.result, Reg::any())?;
                        let arg = use_reg!(right, Reg::any())?;
                        emit!(
                            "    {} {}, {}",
                            op.mnemonic(stmt.result.is_signed()),
                            arg.n(size),
                            dst.n(size)
                        );
                    }
                }
            }
            IrOp::LoadAddr { addr } => {
                let dst = alloc_reg!(stmt.result, Reg::any())?;
                load_addr(&mut out, addr, dst, strings)?;
            }
            IrOp::MemAssign { src, dst } => {
                assert!(dst.is_ref());

                let mut bytes_left = dst.deref_size();
                let dst = use_reg!(dst, Reg::rdi())?;

                let mut maybe_rcx = None;
                let mut maybe_rax = None;

                if src.is_ref() {
                    assert!(src.deref_size() <= bytes_left);

                    let _rsi = use_reg!(src, Reg::rsi())?;
                    let rcx = alloc_tmp_reg!(Reg::rcx())?;

                    emit!("    mov $0x{:x}, {}", src.deref_size(), rcx.n8());
                    bytes_left -= src.deref_size();
                    emit!("    rep movsb");

                    maybe_rcx = Some(Ok(rcx));
                } else if bytes_left >= 8 {
                    assert!(src.size() == 1 || src.size() == 8);

                    let reg = use_reg!(src, Reg::rax())?;
                    if src.size() == 1 {
                        emit!("    movzx {}, {}", reg.n1(), reg.n8());
                    }
                    emit!("    movq {}, ({})", reg.n8(), dst.n8());
                    emit!("    addq $8, {}", dst.n8());
                    bytes_left -= 8;

                    maybe_rax = Some(Ok(reg));
                } else if bytes_left >= 1 {
                    assert!(src.size() == 1);

                    let reg = use_reg!(src, Reg::rax())?;
                    emit!("    movb {}, ({})", reg.n1(), dst.n8());
                    emit!("    incq {}", dst.n8());
                    bytes_left -= 1;

                    maybe_rax = Some(Ok(reg));
                }

                if bytes_left > 0 {
                    // memset to zero
                    let rcx = maybe_rcx.unwrap_or_else(|| alloc_tmp_reg!(Reg::rcx()))?;
                    let rax = maybe_rax.unwrap_or_else(|| alloc_tmp_reg!(Reg::rax()))?;

                    emit!("    mov $0x{:x}, {}", bytes_left, rcx.n8());
                    emit!("    xorb {}, {}", rax.n1(), rax.n1());
                    emit!("    rep stosb")
                }
            }
            IrOp::MemLoad { mem_src } => {
                let dst = alloc_reg!(stmt.result, Reg::any())?;
                let mem = use_reg!(mem_src, Reg::any())?;
                emit!("    mov ({}), {}", mem.n8(), dst.n(stmt.result.size()));
            }
            IrOp::BranchCond {
                cond,
                true_branch,
                false_branch,
            } => {
                let reg = use_reg!(cond, Reg::any())?.n(cond.size());
                emit!("    test {}, {}", reg, reg);
                emit!("    jnz {}", true_branch.to_label(strings));
                emit!("    jmp {}", false_branch.to_label(strings));
            }
            IrOp::EnterScopePlaceholder => unreachable!(),
            IrOp::EnterScope(ref allocator) => {
                stack.push((allocator.spill_space, RegisterState::new()));

                emit!("{}:", allocator.name.to_label(strings));
                emit!("    pushq %rbp");
                emit!("    movq %rsp, %rbp");

                match &allocator.allocator {
                    VariableAllocatorImpl::Global { globals: tmp } => {
                        globals.extend(tmp);
                    }
                    VariableAllocatorImpl::Local { stack_size, .. } => {
                        if *stack_size > 0 {
                            emit!("    subq $0x{:x}, %rsp", stack_size);
                        }
                    }
                }
            }
            IrOp::LeaveScope(name) => {
                emit!("{}:", name.to_label(strings));
                emit!("    movq %rbp, %rsp");
                emit!("    popq %rbp");
                emit!("    ret");
                stack.pop();
                continue;
            }
            IrOp::JmpLabel { name } => {
                emit!("    jmp {}", name.to_label(strings));
            }
            IrOp::Label(name) => {
                emit!("{}:", name.to_label(strings));
            }
            IrOp::ProvideArg(arg) => {
                let value = use_reg!(arg, Reg::any())?;
                emit!("    pushq {}", value.n8());
            }
            IrOp::Call { func, arg_size } => {
                let do_spill = regs!().has_live_regs();
                let _res = alloc_reg!(stmt.result, Reg::rax())?;

                if !do_spill {
                    emit!("    call {}", func.to_label(strings));
                } else {
                    let tmp = alloc_tmp_reg!(Reg::any())?;
                    load_addr(&mut out, spill_addr!(), tmp, strings)?;
                    regs!().spill(&mut out, tmp, Reg::rax() + tmp)?;

                    emit!("    call {}", func.to_label(strings));

                    load_addr(&mut out, spill_addr!(), tmp, strings)?;
                    regs!().recover(&mut out, tmp, Reg::rax() + tmp)?;
                }

                if arg_size > 0 {
                    emit!("    addq $0x{:x}, %rsp", arg_size);
                }
            }
            IrOp::Return { to, value } => {
                let _reg = use_reg!(value, Reg::rax())?;
                emit!("    jmp {}", to.to_label(strings));
            }
            IrOp::Nop(reg) => {
                reuse_reg!(reg, stmt.result, Reg::any())?;
            }
            IrOp::Extend { operand } => {
                assert!(stmt.result.size() == 8);

                let reg = reuse_reg!(operand, stmt.result, Reg::any())?;

                if operand.is_ref() {
                    assert!(operand.deref_size() <= 8);

                    let tmp = alloc_tmp_reg!(Reg::any())?;
                    deref_small_blob(&mut out, reg, tmp, operand.deref_size())?;
                } else if operand.size() != 8 {
                    assert!(operand.size() == 1);

                    // TODO: I am not sure if we need this.
                    // It is only ever useful if we had small sized types, but right now we do not have them.
                    // let insn = if stmt.result.is_signed() {
                    //     "movsx"
                    // } else {
                    //     "movzx"
                    // };
                    let insn = "movzx";
                    emit!("    {} {}, {}", insn, reg.n1(), reg.n8());
                }
            }
        }

        regs!().release_transaction();
    }

    emit!("\n\n.Ldo_exit:");
    emit!("    xorq %rax, %rax");
    emit!("    pushq %rax");
    emit!("    pushq %rax"); // fake the call and fall through ..

    emit!("\n\n.globl exit");
    emit!("exit:");
    emit!("    movabsq $231, %rax");
    emit!("    movq 8(%rsp), %rdi");
    emit!("    syscall");

    emit!("\n\n.globl syscall");
    emit!("syscall:");
    emit!("    movq 8(%rsp), %r9");
    emit!("    movq 16(%rsp), %r8");
    emit!("    movq 24(%rsp), %r10");
    emit!("    movq 32(%rsp), %rdx");
    emit!("    movq 40(%rsp), %rsi");
    emit!("    movq 48(%rsp), %rdi");
    emit!("    movq 56(%rsp), %rax");
    emit!("    syscall");
    emit!("    retq");

    emit!("\n\n.section .rodata");
    emit!("    .align 8\n");
    for (id, name) in ir.constants.iter() {
        emit!("{}:", name.to_label(strings));
        for byte in strings.get(*id).as_bytes() {
            emit!("    .byte 0x{:02x}", byte);
        }
    }

    emit!("\n\n.section .bss");
    emit!("    .align 8\n");
    for (name, size) in globals {
        emit!("{}:", name.to_label(strings));
        emit!("    .skip {}, 0x00\n", size);
    }

    Ok(())
}
