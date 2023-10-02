.section .text

.globl _start
_start:
    lea .Ldo_exit(%rip), %rax
    pushq %rax
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: Internal(32), spill_space: Some(Global { name: Internal(40) }), allocator: Global { globals: [(User(12), 18), (User(14), 23), (User(16), 14), (Internal(40), 72), (User(18), 8)] } })
.Ltmp32:
    pushq %rbp
    movq %rsp, %rbp
    # VReg(0) of Type{void} = JmpLabel { name: Internal(1) }
    jmp .Ltmp1
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: User(1), spill_space: Some(Local(72)), allocator: Local { stack_size: 72, param_size: 16 } })
__print:
    pushq %rbp
    movq %rsp, %rbp
    subq $0x48, %rsp
    # VReg(25 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(25 signed [8]))
    pushq %rax
    # VReg(26 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(26 signed [8]))
    pushq %rax
    # VReg(28 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(8) }
    lea 24(%rbp), %rax
    # VReg(27 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(28 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(27 signed [8]))
    pushq %rbx
    # VReg(30 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rax
    # VReg(29 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(30 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(29 signed [8]))
    pushq %rbx
    # VReg(31 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(31 signed [8]))
    pushq %rax
    # VReg(32 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(32 signed [8]))
    pushq %rax
    # VReg(33 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(33 signed [8]))
    pushq %rax
    # VReg(15 signed [8]) of Type{s64[8, true]} = Call { func: Builtin(4), arg_size: 56 }
    call syscall
    addq $0x38, %rsp
    # VReg(0) of Type{void} = Kill(VReg(15 signed [8]))
    # VReg(0) of Type{void} = LeaveScope(EndUser(1))
.Lend__print:
    movq %rbp, %rsp
    popq %rbp
    ret
    # VReg(0) of Type{void} = Label(Internal(1))
.Ltmp1:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(2) }
    jmp .Ltmp2
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: User(5), spill_space: Some(Local(72)), allocator: Local { stack_size: 72, param_size: 16 } })
__read:
    pushq %rbp
    movq %rsp, %rbp
    subq $0x48, %rsp
    # VReg(48 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(48 signed [8]))
    pushq %rax
    # VReg(49 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(49 signed [8]))
    pushq %rax
    # VReg(51 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(8) }
    lea 24(%rbp), %rax
    # VReg(50 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(51 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(50 signed [8]))
    pushq %rbx
    # VReg(53 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Argument(0) }
    lea 16(%rbp), %rax
    # VReg(52 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(53 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = ProvideArg(VReg(52 signed [8]))
    pushq %rbx
    # VReg(54 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(54 signed [8]))
    pushq %rax
    # VReg(55 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(55 signed [8]))
    pushq %rax
    # VReg(56 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(56 signed [8]))
    pushq %rax
    # VReg(38 signed [8]) of Type{s64[8, true]} = Call { func: Builtin(4), arg_size: 56 }
    call syscall
    addq $0x38, %rsp
    # VReg(0) of Type{void} = Kill(VReg(38 signed [8]))
    # VReg(0) of Type{void} = LeaveScope(EndUser(5))
.Lend__read:
    movq %rbp, %rsp
    popq %rbp
    ret
    # VReg(0) of Type{void} = Label(Internal(2))
.Ltmp2:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(3) }
    jmp .Ltmp3
    # VReg(0) of Type{void} = EnterScope(VariableAllocator { name: User(6), spill_space: Some(Local(96)), allocator: Local { stack_size: 112, param_size: 0 } })
__read_number:
    pushq %rbp
    movq %rsp, %rbp
    subq $0x70, %rsp
    # VReg(67 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: Internal(35) } }
    lea .Ltmp35(%rip), %rax
    # VReg(69 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rbx
    # VReg(68 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = Nop(VReg(69 ref [*10]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(67 ref [*10]), dst: VReg(68 ref [*10]) }
    movq %rbx, %rdi
    movq %rax, %rsi
    mov $0xa, %rcx
    rep movsb
    # VReg(71 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(73 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(72 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(73 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(71 signed [8]), dst: VReg(72 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(0) of Type{void} = Label(Internal(5))
.Ltmp5:
    # VReg(88 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rax
    # VReg(87 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(88 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(255 [8]) of Type{-ptr-> Array[10 x Type{u8[1, false]}]} = Extend { operand: VReg(87 signed [8]) }
    # VReg(89 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rax
    # VReg(86 [8]) of Type{-ptr-> Array[10 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(89 ref [*10]) }
    # VReg(85 [8]) of Type{-ptr-> Array[10 x Type{u8[1, false]}]} = Binary { op: Add, left: VReg(86 [8]), right: VReg(255 [8]) }
    add %rbx, %rax
    # VReg(256 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(85 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(256 signed [8]))
    pushq %rax
    # VReg(90 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(90 signed [8]))
    pushq %rax
    # VReg(80 [0]) of Type{void} = Call { func: User(5), arg_size: 16 }
    call __read
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(80 [0]))
    # VReg(104 [1]) of Type{u8[1, false]} = LoadConst { value: 58 }
    mov $0x3a, %al
    # VReg(108 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(106 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(108 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(107 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rbx
    # VReg(105 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(107 [8]), right: VReg(106 signed [8]) }
    add %cl, %bl
    # VReg(103 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(105 ref [*1]) }
    mov (%rbx), %cl
    # VReg(102 [1]) of Type{u8[1, false]} = Binary { op: Lt, left: VReg(103 [1]), right: VReg(104 [1]) }
    sub %al, %cl
    setb %bl
    # VReg(101 [1]) of Type{u8[1, false]} = Unary { op: Not, operand: VReg(102 [1]) }
    and %bl, %bl
    sete %bl
    # VReg(110 [1]) of Type{u8[1, false]} = LoadConst { value: 48 }
    mov $0x30, %al
    # VReg(114 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rcx
    # VReg(112 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(114 signed ref [*8]) }
    mov (%rcx), %rdx
    # VReg(113 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rcx
    # VReg(111 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(113 [8]), right: VReg(112 signed [8]) }
    add %dl, %cl
    # VReg(109 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(111 ref [*1]) }
    mov (%rcx), %dl
    # VReg(100 [1]) of Type{u8[1, false]} = Binary { op: Lt, left: VReg(109 [1]), right: VReg(110 [1]) }
    sub %al, %dl
    setb %cl
    # VReg(99 [1]) of Type{u8[1, false]} = Binary { op: BitOr, left: VReg(100 [1]), right: VReg(101 [1]) }
    or %bl, %cl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(99 [1]), true_branch: Internal(7), false_branch: Internal(8) }
    test %cl, %cl
    jnz .Ltmp7
    jmp .Ltmp8
    # VReg(0) of Type{void} = Label(Internal(7))
.Ltmp7:
    # VReg(0) of Type{void} = JmpLabel { name: EndInternal(5) }
    jmp .Lend_5
    # VReg(0) of Type{void} = Label(EndInternal(7))
.Lend_7:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(6) }
    jmp .Ltmp6
    # VReg(0) of Type{void} = Label(Internal(8))
.Ltmp8:
    # VReg(0) of Type{void} = Label(EndInternal(8))
.Lend_8:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(6) }
    jmp .Ltmp6
    # VReg(0) of Type{void} = Label(Internal(6))
.Ltmp6:
    # VReg(120 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(121 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(119 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(121 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(116 signed [8]) of Type{s64[8, true]} = Binary { op: Add, left: VReg(119 signed [8]), right: VReg(120 signed [8]) }
    add %rax, %rcx
    # VReg(118 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rax
    # VReg(117 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(118 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(116 signed [8]), dst: VReg(117 signed ref [*8]) }
    movq %rax, %rdi
    movq %rcx, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(133 signed [8]) of Type{s64[8, true]} = LoadConst { value: 10 }
    mov $0xa, %rax
    # VReg(134 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(132 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(134 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(131 [1]) of Type{u8[1, false]} = Binary { op: Lt, left: VReg(132 signed [8]), right: VReg(133 signed [8]) }
    sub %rax, %rcx
    setl %bl
    # VReg(130 [1]) of Type{u8[1, false]} = Unary { op: Not, operand: VReg(131 [1]) }
    and %bl, %bl
    sete %bl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(130 [1]), true_branch: Internal(10), false_branch: Internal(11) }
    test %bl, %bl
    jnz .Ltmp10
    jmp .Ltmp11
    # VReg(0) of Type{void} = Label(Internal(10))
.Ltmp10:
    # VReg(0) of Type{void} = JmpLabel { name: EndInternal(5) }
    jmp .Lend_5
    # VReg(0) of Type{void} = Label(EndInternal(10))
.Lend_10:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(9) }
    jmp .Ltmp9
    # VReg(0) of Type{void} = Label(Internal(11))
.Ltmp11:
    # VReg(0) of Type{void} = Label(EndInternal(11))
.Lend_11:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(9) }
    jmp .Ltmp9
    # VReg(0) of Type{void} = Label(Internal(9))
.Ltmp9:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(5) }
    jmp .Ltmp5
    # VReg(0) of Type{void} = Label(EndInternal(5))
.Lend_5:
    # VReg(136 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(138 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(104) }
    lea -104(%rbp), %rbx
    # VReg(137 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(138 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(136 signed [8]), dst: VReg(137 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(140 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(142 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(112) }
    lea -112(%rbp), %rbx
    # VReg(141 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(142 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(140 signed [8]), dst: VReg(141 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(0) of Type{void} = Label(Internal(12))
.Ltmp12:
    # VReg(160 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(161 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(159 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(161 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(158 [1]) of Type{u8[1, false]} = Binary { op: Eq, left: VReg(159 signed [8]), right: VReg(160 signed [8]) }
    xor %rcx, %rax
    sete %bl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(158 [1]), true_branch: Internal(14), false_branch: Internal(15) }
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
    # VReg(167 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(168 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(166 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(168 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(163 signed [8]) of Type{s64[8, true]} = Binary { op: Sub, left: VReg(166 signed [8]), right: VReg(167 signed [8]) }
    sub %rax, %rcx
    # VReg(165 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rax
    # VReg(164 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(165 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(163 signed [8]), dst: VReg(164 signed ref [*8]) }
    movq %rax, %rdi
    movq %rcx, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(177 [1]) of Type{u8[1, false]} = LoadConst { value: 48 }
    mov $0x30, %al
    # VReg(181 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(24) }
    lea -24(%rbp), %rbx
    # VReg(179 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(181 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(180 ref [*10]) of Type{-ref-> Array[10 x Type{u8[1, false]}]} = LoadAddr { addr: Local(16) }
    lea -16(%rbp), %rbx
    # VReg(178 ref [*1]) of Type{-ref-> u8[1, false]} = Binary { op: Add, left: VReg(180 [8]), right: VReg(179 signed [8]) }
    add %cl, %bl
    # VReg(176 [1]) of Type{u8[1, false]} = MemLoad { mem_src: VReg(178 ref [*1]) }
    mov (%rbx), %cl
    # VReg(175 [1]) of Type{u8[1, false]} = Binary { op: Sub, left: VReg(176 [1]), right: VReg(177 [1]) }
    sub %al, %cl
    # VReg(257 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(175 [1]) }
    movzx %cl, %rcx
    # VReg(182 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(104) }
    lea -104(%rbp), %rax
    # VReg(174 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(182 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(173 signed [8]) of Type{s64[8, true]} = Binary { op: Mul, left: VReg(174 signed [8]), right: VReg(257 signed [8]) }
    movq %rbx, %rax
    imul %rcx
    # VReg(183 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(112) }
    lea -112(%rbp), %rbx
    # VReg(172 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(183 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(169 signed [8]) of Type{s64[8, true]} = Binary { op: Add, left: VReg(172 signed [8]), right: VReg(173 signed [8]) }
    add %rax, %rcx
    # VReg(171 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(112) }
    lea -112(%rbp), %rax
    # VReg(170 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(171 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(169 signed [8]), dst: VReg(170 signed ref [*8]) }
    movq %rax, %rdi
    movq %rcx, %rax
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(188 signed [8]) of Type{s64[8, true]} = LoadConst { value: 10 }
    mov $0xa, %rax
    # VReg(189 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(104) }
    lea -104(%rbp), %rbx
    # VReg(187 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(189 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(184 signed [8]) of Type{s64[8, true]} = Binary { op: Mul, left: VReg(187 signed [8]), right: VReg(188 signed [8]) }
    xchgq %rax, %rcx
    imul %rcx
    # VReg(186 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(104) }
    lea -104(%rbp), %rbx
    # VReg(185 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(186 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(184 signed [8]), dst: VReg(185 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(0) of Type{void} = JmpLabel { name: Internal(12) }
    jmp .Ltmp12
    # VReg(0) of Type{void} = Label(EndInternal(12))
.Lend_12:
    # VReg(191 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Local(112) }
    lea -112(%rbp), %rax
    # VReg(190 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(191 signed ref [*8]) }
    mov (%rax), %rbx
    # VReg(0) of Type{void} = Return { to: EndUser(6), value: VReg(190 signed [8]) }
    movq %rbx, %rax
    jmp .Lend__read_number
    # VReg(0) of Type{void} = LeaveScope(EndUser(6))
.Lend__read_number:
    movq %rbp, %rsp
    popq %rbp
    ret
    # VReg(0) of Type{void} = Label(Internal(3))
.Ltmp3:
    # VReg(192 ref [*18]) of Type{-ref-> Array[18 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: Internal(37) } }
    lea .Ltmp37(%rip), %rax
    # VReg(194 ref [*18]) of Type{-ref-> Array[18 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: User(12) } }
    lea __prompt(%rip), %rbx
    # VReg(193 ref [*18]) of Type{-ref-> Array[18 x Type{u8[1, false]}]} = Nop(VReg(194 ref [*18]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(192 ref [*18]), dst: VReg(193 ref [*18]) }
    movq %rbx, %rdi
    movq %rax, %rsi
    mov $0x12, %rcx
    rep movsb
    # VReg(196 ref [*23]) of Type{-ref-> Array[23 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: Internal(38) } }
    lea .Ltmp38(%rip), %rax
    # VReg(198 ref [*23]) of Type{-ref-> Array[23 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: User(14) } }
    lea __win(%rip), %rbx
    # VReg(197 ref [*23]) of Type{-ref-> Array[23 x Type{u8[1, false]}]} = Nop(VReg(198 ref [*23]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(196 ref [*23]), dst: VReg(197 ref [*23]) }
    movq %rbx, %rdi
    movq %rax, %rsi
    mov $0x17, %rcx
    rep movsb
    # VReg(200 ref [*14]) of Type{-ref-> Array[14 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: Internal(39) } }
    lea .Ltmp39(%rip), %rax
    # VReg(202 ref [*14]) of Type{-ref-> Array[14 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: User(16) } }
    lea __fail(%rip), %rbx
    # VReg(201 ref [*14]) of Type{-ref-> Array[14 x Type{u8[1, false]}]} = Nop(VReg(202 ref [*14]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(200 ref [*14]), dst: VReg(201 ref [*14]) }
    movq %rbx, %rdi
    movq %rax, %rsi
    mov $0xe, %rcx
    rep movsb
    # VReg(209 ref [*18]) of Type{-ref-> Array[18 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: User(12) } }
    lea __prompt(%rip), %rax
    # VReg(208 [8]) of Type{-ptr-> Array[18 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(209 ref [*18]) }
    # VReg(258 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(208 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(258 signed [8]))
    pushq %rax
    # VReg(210 signed [8]) of Type{s64[8, true]} = LoadConst { value: 18 }
    mov $0x12, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(210 signed [8]))
    pushq %rax
    # VReg(4 [0]) of Type{void} = Call { func: User(1), arg_size: 16 }
    call __print
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(4 [0]))
    # VReg(211 signed [8]) of Type{s64[8, true]} = Call { func: User(6), arg_size: 0 }
    call __read_number
    # VReg(213 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Global { name: User(18) } }
    lea __number(%rip), %rbx
    # VReg(212 signed ref [*8]) of Type{-ref-> s64[8, true]} = Nop(VReg(213 signed ref [*8]))
    # VReg(0) of Type{void} = MemAssign { src: VReg(211 signed [8]), dst: VReg(212 signed ref [*8]) }
    movq %rbx, %rdi
    movq %rax, (%rdi)
    addq $8, %rdi
    # VReg(227 signed [8]) of Type{s64[8, true]} = LoadConst { value: 4919 }
    mov $0x1337, %rax
    # VReg(228 signed ref [*8]) of Type{-ref-> s64[8, true]} = LoadAddr { addr: Global { name: User(18) } }
    lea __number(%rip), %rbx
    # VReg(226 signed [8]) of Type{s64[8, true]} = MemLoad { mem_src: VReg(228 signed ref [*8]) }
    mov (%rbx), %rcx
    # VReg(225 [1]) of Type{u8[1, false]} = Binary { op: Eq, left: VReg(226 signed [8]), right: VReg(227 signed [8]) }
    xor %rcx, %rax
    sete %bl
    # VReg(0) of Type{void} = BranchCond { cond: VReg(225 [1]), true_branch: Internal(20), false_branch: Internal(21) }
    test %bl, %bl
    jnz .Ltmp20
    jmp .Ltmp21
    # VReg(0) of Type{void} = Label(Internal(20))
.Ltmp20:
    # VReg(236 ref [*23]) of Type{-ref-> Array[23 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: User(14) } }
    lea __win(%rip), %rax
    # VReg(235 [8]) of Type{-ptr-> Array[23 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(236 ref [*23]) }
    # VReg(260 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(235 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(260 signed [8]))
    pushq %rax
    # VReg(237 signed [8]) of Type{s64[8, true]} = LoadConst { value: 23 }
    mov $0x17, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(237 signed [8]))
    pushq %rax
    # VReg(230 [0]) of Type{void} = Call { func: User(1), arg_size: 16 }
    call __print
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(230 [0]))
    # VReg(241 signed [8]) of Type{s64[8, true]} = LoadConst { value: 0 }
    mov $0x0, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(241 signed [8]))
    pushq %rax
    # VReg(229 [0]) of Type{void} = Call { func: Builtin(19), arg_size: 8 }
    call exit
    addq $0x8, %rsp
    # VReg(0) of Type{void} = Kill(VReg(229 [0]))
    # VReg(0) of Type{void} = Label(EndInternal(20))
.Lend_20:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(19) }
    jmp .Ltmp19
    # VReg(0) of Type{void} = Label(Internal(21))
.Ltmp21:
    # VReg(249 ref [*14]) of Type{-ref-> Array[14 x Type{u8[1, false]}]} = LoadAddr { addr: Global { name: User(16) } }
    lea __fail(%rip), %rax
    # VReg(248 [8]) of Type{-ptr-> Array[14 x Type{u8[1, false]}]} = Unary { op: AddrOf, operand: VReg(249 ref [*14]) }
    # VReg(259 signed [8]) of Type{s64[8, true]} = Extend { operand: VReg(248 [8]) }
    # VReg(0) of Type{void} = ProvideArg(VReg(259 signed [8]))
    pushq %rax
    # VReg(250 signed [8]) of Type{s64[8, true]} = LoadConst { value: 14 }
    mov $0xe, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(250 signed [8]))
    pushq %rax
    # VReg(243 [0]) of Type{void} = Call { func: User(1), arg_size: 16 }
    call __print
    addq $0x10, %rsp
    # VReg(0) of Type{void} = Kill(VReg(243 [0]))
    # VReg(254 signed [8]) of Type{s64[8, true]} = LoadConst { value: 1 }
    mov $0x1, %rax
    # VReg(0) of Type{void} = ProvideArg(VReg(254 signed [8]))
    pushq %rax
    # VReg(242 [0]) of Type{void} = Call { func: Builtin(19), arg_size: 8 }
    call exit
    addq $0x8, %rsp
    # VReg(0) of Type{void} = Kill(VReg(242 [0]))
    # VReg(0) of Type{void} = Label(EndInternal(21))
.Lend_21:
    # VReg(0) of Type{void} = JmpLabel { name: Internal(19) }
    jmp .Ltmp19
    # VReg(0) of Type{void} = Label(Internal(19))
.Ltmp19:
    # VReg(0) of Type{void} = LeaveScope(EndInternal(32))
.Lend_32:
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

.Ltmp35:
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
    .byte 0x00
.Ltmp37:
    .byte 0x67
    .byte 0x69
    .byte 0x76
    .byte 0x65
    .byte 0x20
    .byte 0x6d
    .byte 0x65
    .byte 0x20
    .byte 0x61
    .byte 0x20
    .byte 0x6e
    .byte 0x75
    .byte 0x6d
    .byte 0x62
    .byte 0x65
    .byte 0x72
    .byte 0x3a
    .byte 0x20
.Ltmp38:
    .byte 0x79
    .byte 0x6f
    .byte 0x75
    .byte 0x20
    .byte 0x67
    .byte 0x75
    .byte 0x65
    .byte 0x73
    .byte 0x73
    .byte 0x65
    .byte 0x64
    .byte 0x20
    .byte 0x63
    .byte 0x6f
    .byte 0x72
    .byte 0x72
    .byte 0x65
    .byte 0x63
    .byte 0x74
    .byte 0x6c
    .byte 0x79
    .byte 0x21
    .byte 0x0a
.Ltmp39:
    .byte 0x76
    .byte 0x65
    .byte 0x72
    .byte 0x79
    .byte 0x20
    .byte 0x77
    .byte 0x72
    .byte 0x6f
    .byte 0x6e
    .byte 0x67
    .byte 0x20
    .byte 0x3a
    .byte 0x28
    .byte 0x0a


.section .bss
    .align 8

__prompt:
    .skip 18, 0x00

__win:
    .skip 23, 0x00

__fail:
    .skip 14, 0x00

.Ltmp40:
    .skip 72, 0x00

__number:
    .skip 8, 0x00

