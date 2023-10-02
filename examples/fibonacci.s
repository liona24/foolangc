.section .text

.globl _start
_start:
    lea .Ldo_exit(%rip), %rax
    pushq %rax
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: Internal(24), spill_space: Some(Global { name: Internal(30) }), allocator: Global { globals: [(Internal(30), 72), (User(16), 8)] } })
.Ltmp24:
    pushq %rbp
    movq %rsp, %rbp
    # VReg(0) of Type{void} = JmpLabel { name: Internal(1) }
    jmp .Ltmp1
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: User(1), spill_space: Some(Local(72)), allocator: Local { stack_size: 72, param_size: 16 } })
__print:
    pushq %rbp
    movq %rsp, %rbp
    subq $0x48, %rsp
    # VReg(21 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(21 signed [8]))
    pushq %rax
    # VReg(22 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(22 signed [8]))
    pushq %rax
    # VReg(24 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(8) }
    lea 24(%rbp), %rax
    # VReg(23 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(24 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(23 signed [8]))
    pushq %rbx
    # VReg(26 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rax
    # VReg(25 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(26 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(25 signed [8]))
    pushq %rbx
    # VReg(27 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(27 signed [8]))
    pushq %rax
    # VReg(28 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(28 signed [8]))
    pushq %rax
    # VReg(29 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(29 signed [8]))
    pushq %rax
    # VReg(11 signed [8]) of Type{s64[8, true]} = Call { func: Builtin(4), arg_size: 56 }
    call syscall
    addq $0x38, %rsp
    # VReg(0) of Type{void} = Kill(VReg(11 signed [8]))
    # VReg(0) of Type{void} = LeaveScope(EndUser(1))
.Lend__print:
    movq %rbp, %rsp
    popq %rbp
    ret
    # VReg(0) of Type{void} = Label(Internal(1))
.Ltmp1:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(2) }
    jmp .Ltmp2
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: User(5), spill_space: Some(Local(72)), allocator: Local { stack_size: 72, param_size: 8 } })
__fib:
    pushq %rbp
    movq %rsp, %rbp
    subq $0x48, %rsp
    # VReg(45 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(46 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rbx
    # VReg(44 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(46 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(43 [1]) of Type{u8[1, false]} = Binary { op: Eq, left: VReg(44 signed [8]), right: VReg(45 signed [8]) }
    xor %rcx, %rax
    sete %bl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(43 [1]), true_branch: Internal(4), false_branch: Internal(5) }
    test %bl, %bl
    jnz .Ltmp4
    jmp .Ltmp5
    # VReg(0) of Type{void} = Label(Internal(4))
.Ltmp4:
    # VReg(48 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = Return { to: EndUser(5), value: VReg(48 signed [8]) }
    jmp .Lend__fib
    # VReg(0) of Type{void} = Label(EndInternal(4))
.Lend_4:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(3) }
    jmp .Ltmp3
    # VReg(0) of Type{void} = Label(Internal(5))
.Ltmp5:
    # VReg(59 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(60 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rbx
    # VReg(58 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(60 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(57 [1]) of Type{u8[1, false]} = Binary { op: Eq, left: VReg(58 signed [8]), right: VReg(59 signed [8]) }
    xor %rcx, %rax
    sete %bl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(57 [1]), true_branch: Internal(7), false_branch: Internal(8) }
    test %bl, %bl
    jnz .Ltmp7
    jmp .Ltmp8
    # VReg(0) of Type{void} = Label(Internal(7))
.Ltmp7:
    # VReg(62 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = Return { to: EndUser(5), value: VReg(62 signed [8]) }
    jmp .Lend__fib
    # VReg(0) of Type{void} = Label(EndInternal(7))
.Lend_7:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(6) }
    jmp .Ltmp6
    # VReg(0) of Type{void} = Label(Internal(8))
.Ltmp8:
    # VReg(72 signed [8]) of Type{s64[8, true]} = LoadConst { value: 2 }
    mov $0x2, %rax
    # VReg(73 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rbx
    # VReg(71 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(73 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(70 signed [8]) of Type{s64[8, true]} = Binary { op: Sub, left: VReg(71 signed [8]), right: VReg(72 signed [8]) }
    sub %rax, %rcx
    # VReg(0) of Type{void} = ProvideArg(VReg(70 signed [8]))
    pushq %rcx
    # VReg(66 signed [8]) of Type{s64[8, true]} = Call { func: User(5), arg_size: 8 }
    call __fib
    addq $0x8, %rsp
    # VReg(79 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rbx
    # VReg(80 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rcx
    # VReg(78 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(80 signed ref [*8]) }
    mov (%rcx), %rdx
    # VReg(77 signed [8]) of Type{s64[8, true]} = Binary { op: Sub, left: VReg(78 signed [8]), right: VReg(79 signed [8]) }
    sub %rbx, %rdx
    # VReg(0) of Type{void} = ProvideArg(VReg(77 signed [8]))
    pushq %rdx
    # VReg(65 signed [8]) of Type{s64[8, true]} = Call { func: User(5), arg_size: 8 }
    movq %rax, %rbx
    lea -72(%rbp), %rcx
    movq %rbx, 8(%rcx)
    call __fib
    lea -72(%rbp), %rcx
    movq 8(%rcx), %rbx
    addq $0x8, %rsp
    # VReg(64 signed [8]) of Type{s64[8, true]} = Binary { op: Add, left: VReg(65 signed [8]), right: VReg(66 signed [8]) }
    add %rbx, %rax
    # VReg(0) of Type{void} = Return { to: EndUser(5), value: VReg(64 signed [8]) }
    jmp .Lend__fib
    # VReg(0) of Type{void} = Label(EndInternal(8))
.Lend_8:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(6) }
    jmp .Ltmp6
    # VReg(0) of Type{void} = Label(Internal(6))
.Ltmp6:
    # VReg(0) of Type{void} = Label(EndInternal(5))
.Lend_5:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(3) }
    jmp .Ltmp3
    # VReg(0) of Type{void} = Label(Internal(3))
.Ltmp3:
    # VReg(0) of Type{void} = LeaveScope(EndUser(5))
.Lend__fib:
    movq %rbp, %rsp
    popq %rbp
    ret
    # VReg(0) of Type{void} = Label(Internal(2))
.Ltmp2:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(9) }
    jmp .Ltmp9
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: User(7), spill_space: Some(Local(128)), allocator: Local { stack_size: 128, param_size: 8 } })
__print_number:
    pushq %rbp
    movq %rsp, %rbp
    subq $0x80, %rsp
    # VReg(89 ref [*16]) of Type{-ref-> Array[16 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: Internal(27) } }
    lea .Ltmp27(%rip), %rax
    # VReg(91 ref [*16]) of Type{-ref-> Array[16 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rbx
    # VReg(90 ref [*16]) of Type{-ref-> Array[16 x Type{u8[1, false]}]} = Nop(VReg(91 ref [*16]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(89 ref [*16]), dst: VReg(90 ref [*16]) }
    movq %rbx, %rdi
    movq %rax, %rsi
    mov $0x10, %rcx
    rep movsb
    # VReg(97 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rax
    # VReg(93 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(97 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(95 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rax
    # VReg(94 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = Nop(VReg(95 ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(93 signed [8]), dst: VReg(94 ref [*8]) }
    movq %rax, %rdi
    movq %rbx, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(98 signed [8]) of Type{s64[8, true]} = LoadConst { value: 7 }
    mov $0x7, %rax
    # VReg(100 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(32) }
    lea -32(%rbp), %rbx
    # VReg(99 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(100 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(98 signed [8]), dst: VReg(99 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(0) of Type{void} = Label(Internal(12))
.Ltmp12:
    # VReg(121 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(32) }
    lea -32(%rbp), %rax
    # VReg(119 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(121 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(120 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rax
    # VReg(118 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(120 [8]), right: VReg(119 signed [8]) }
    add %bl, %al
    # VReg(117 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(118 ref [*1]) }
    mov (%rax), %bl
    # VReg(203 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(117 [1]) }
    movzx %bl, %rbx
    # VReg(116 signed [8]) of Type{s64[8, true]} = LoadConst { value: 16 }
    mov $0x10, %rax
    # VReg(112 signed [8]) of Type{s64[8, true]} = Binary { op: Mul, left: VReg(116 signed [8]), right: VReg(203 signed [8]) }
    imul %rbx
    # VReg(114 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(40) }
    lea -40(%rbp), %rbx
    # VReg(113 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = Nop(VReg(114 ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(112 signed [8]), dst: VReg(113 ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(127 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(128 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(40) }
    lea -40(%rbp), %rbx
    # VReg(126 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(128 [8]), right: VReg(127 signed [8]) }
    add %al, %bl
    # VReg(122 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(126 ref [*1]) }
    mov (%rbx), %al
    # VReg(124 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(48) }
    lea -48(%rbp), %rbx
    # VReg(123 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(124 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(122 [1]), dst: VReg(123 signed ref [*8]) }
    movq %rbx, %rdi
    movzx %al, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(135 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(136 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(40) }
    lea -40(%rbp), %rbx
    # VReg(134 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(136 [8]), right: VReg(135 signed [8]) }
    add %al, %bl
    # VReg(133 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(134 ref [*1]) }
    mov (%rbx), %al
    # VReg(204 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(133 [1]) }
    movzx %al, %rax
    # VReg(132 signed [8]) of Type{s64[8, true]} = LoadConst { value: 16 }
    mov $0x10, %rbx
    # VReg(129 signed [8]) of Type{s64[8, true]} = Binary { op: Mul, left: VReg(132 signed [8]), right: VReg(204 signed [8]) }
    xchgq %rax, %rbx
    imul %rbx
    # VReg(131 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(40) }
    lea -40(%rbp), %rbx
    # VReg(130 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = Nop(VReg(131 ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(129 signed [8]), dst: VReg(130 ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(142 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(143 ref [*8]) of Type{-ref-> Array[8 x Type{u8[1, false]}]} = LoadAddr { addr: Local(40) }
    lea -40(%rbp), %rbx
    # VReg(141 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(143 [8]), right: VReg(142 signed [8]) }
    add %al, %bl
    # VReg(137 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(141 ref [*1]) }
    mov (%rbx), %al
    # VReg(139 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(56) }
    lea -56(%rbp), %rbx
    # VReg(138 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(139 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(137 [1]), dst: VReg(138 signed ref [*8]) }
    movq %rbx, %rdi
    movzx %al, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(151 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(48) }
    lea -48(%rbp), %rax
    # VReg(150 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(151 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(205 [8]) of Type{-ptr-> Array[16 x Type{u8[1, false]}]} = Extend { operand: VReg(150 signed [8]) }
    # VReg(152 ref [*16]) of Type{-ref-> Array[16 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rax
    # VReg(149 [8]) of Type{-ptr-> Array[16 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(152 ref [*16]) }
    # VReg(148 [8]) of Type{-ptr-> Array[16 x Type{u8[1, false]}]} = Binary { op: Add, left: VReg(149 [8]), right: VReg(205 [8]) }
    add %rbx, %rax
    # VReg(206 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(148 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(206 signed [8]))
    pushq %rax
    # VReg(153 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(153 signed [8]))
    pushq %rax
    # VReg(107 [0]) of Type{void} = Call { func: User(1), arg_size: 16 }
    call __print
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(107 [0]))
    # VReg(161 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(56) }
    lea -56(%rbp), %rax
    # VReg(160 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(161 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(207 [8]) of Type{-ptr-> Array[16 x Type{u8[1, false]}]} = Extend { operand: VReg(160 signed [8]) }
    # VReg(162 ref [*16]) of Type{-ref-> Array[16 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rax
    # VReg(159 [8]) of Type{-ptr-> Array[16 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(162 ref [*16]) }
    # VReg(158 [8]) of Type{-ptr-> Array[16 x Type{u8[1, false]}]} = Binary { op: Add, left: VReg(159 [8]), right: VReg(207 [8]) }
    add %rbx, %rax
    # VReg(208 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(158 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(208 signed [8]))
    pushq %rax
    # VReg(163 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(163 signed [8]))
    pushq %rax
    # VReg(106 [0]) of Type{void} = Call { func: User(1), arg_size: 16 }
    call __print
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(106 [0]))
    # VReg(168 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(169 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(32) }
    lea -32(%rbp), %rbx
    # VReg(167 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(169 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(164 signed [8]) of Type{s64[8, true]} = Binary { op: Sub, left: VReg(167 signed [8]), right: VReg(168 signed [8]) }
    sub %rax, %rcx
    # VReg(166 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(32) }
    lea -32(%rbp), %rax
    # VReg(165 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(166 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(164 signed [8]), dst: VReg(165 signed ref [*8]) }
    movq %rax, %rdi
    movq %rcx, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(180 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(181 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(32) }
    lea -32(%rbp), %rbx
    # VReg(179 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(181 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(178 [1]) of Type{u8[1, false]} = Binary { op: Lt, left: VReg(179 signed [8]), right: VReg(180 signed [8]) }
    sub %rax, %rcx
    setl %bl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(178 [1]), true_branch: Internal(14), false_branch: Internal(15) }
    test %bl, %bl
    jnz .Ltmp14
    jmp .Ltmp15
    # VReg(0) of Type{void} = Label(Internal(14))
.Ltmp14:
    # VReg(0) of Type{void} = JmpLabel { name: EndInternal(12) }
    jmp .Lend_12
    # VReg(0) of Type{void} = Label(EndInternal(14))
.Lend_14:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(13) }
    jmp .Ltmp13
    # VReg(0) of Type{void} = Label(Internal(15))
.Ltmp15:
    # VReg(0) of Type{void} = Label(EndInternal(15))
.Lend_15:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(13) }
    jmp .Ltmp13
    # VReg(0) of Type{void} = Label(Internal(13))
.Ltmp13:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(12) }
    jmp .Ltmp12
    # VReg(0) of Type{void} = Label(EndInternal(12))
.Lend_12:
    # VReg(188 ref [*1]) of Type{-ref-> Array[1 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: Internal(29) } }
    lea .Ltmp29(%rip), %rax
    # VReg(187 [8]) of Type{-ptr-> Array[1 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(188 ref [*1]) }
    # VReg(209 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(187 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(209 signed [8]))
    pushq %rax
    # VReg(189 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(189 signed [8]))
    pushq %rax
    # VReg(84 [0]) of Type{void} = Call { func: User(1), arg_size: 16 }
    call __print
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(84 [0]))
    # VReg(0) of Type{void} = LeaveScope(EndUser(7))
.Lend__print_number:
    movq %rbp, %rsp
    popq %rbp
    ret
    # VReg(0) of Type{void} = Label(Internal(9))
.Ltmp9:
    # VReg(197 signed [8]) of Type{s64[8, true]} = LoadConst { value: 17 }
    mov $0x11, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(197 signed [8]))
    pushq %rax
    # VReg(190 signed [8]) of Type{s64[8, true]} = Call { func: User(5), arg_size: 8 }
    call __fib
    addq $0x8, %rsp
    # VReg(192 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Global { name: User(16) } }
    lea __x(%rip), %rbx
    # VReg(191 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(192 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(190 signed [8]), dst: VReg(191 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(202 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Global { name: User(16) } }
    lea __x(%rip), %rax
    # VReg(201 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(202 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(201 signed [8]))
    pushq %rbx
    # VReg(2 [0]) of Type{void} = Call { func: User(7), arg_size: 8 }
    call __print_number
    addq $0x8, %rsp
    # VReg(0) of Type{void} = Kill(VReg(2 [0]))
    # VReg(0) of Type{void} = LeaveScope(EndInternal(24))
.Lend_24:
    movq %rbp, %rsp
    popq %rbp
    ret


.Ldo_exit:
    xorq %rax, %rax
    pushq %rax
    pushq %rax


.globl exit
exit:
    movabsq $231, %rax
    movq 8(%rsp), %rdi
    syscall


.globl syscall
syscall:
    movq 8(%rsp), %r9
    movq 16(%rsp), %r8
    movq 24(%rsp), %r10
    movq 32(%rsp), %rdx
    movq 40(%rsp), %rsi
    movq 48(%rsp), %rdi
    movq 56(%rsp), %rax
    syscall
    retq


.section .rodata
    .align 8

.Ltmp27:
    .byte 0x30
    .byte 0x31
    .byte 0x32
    .byte 0x33
    .byte 0x34
    .byte 0x35
    .byte 0x36
    .byte 0x37
    .byte 0x38
    .byte 0x39
    .byte 0x41
    .byte 0x42
    .byte 0x43
    .byte 0x44
    .byte 0x45
    .byte 0x46
.Ltmp29:
    .byte 0x0a


.section .bss
    .align 8

.Ltmp30:
    .skip 72, 0x00

__x:
    .skip 8, 0x00

