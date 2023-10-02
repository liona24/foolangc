use std::collections::BTreeMap;

use super::ir::VReg;

const REG_RAX: u32 = 0;
const REG_RBX: u32 = 1;
const REG_RCX: u32 = 2;
const REG_RDX: u32 = 3;
const REG_RSI: u32 = 4;
const REG_RDI: u32 = 5;
const REG_R10: u32 = 6;
const REG_R11: u32 = 7;
const REG_R12: u32 = 8;

pub const NUM_GP_REGS: u32 = 9;
const REG_ANY_GP: u32 = (1 << REG_RAX)
    | (1 << REG_RBX)
    | (1 << REG_RCX)
    | (1 << REG_RDX)
    | (1 << REG_RSI)
    | (1 << REG_RDI)
    | (1 << REG_R10)
    | (1 << REG_R11)
    | (1 << REG_R12);

const REG_RBP: u32 = 30;
const REG_RSP: u32 = 31;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reg(u32);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct RegSet(u32);

impl Reg {
    #[allow(unused)]
    pub const fn rax() -> Self {
        Self(REG_RAX)
    }
    #[allow(unused)]
    pub const fn rbx() -> Self {
        Self(REG_RBX)
    }
    #[allow(unused)]
    pub const fn rcx() -> Self {
        Self(REG_RCX)
    }
    #[allow(unused)]
    pub const fn rdx() -> Self {
        Self(REG_RDX)
    }
    #[allow(unused)]
    pub const fn rsi() -> Self {
        Self(REG_RSI)
    }
    #[allow(unused)]
    pub const fn rdi() -> Self {
        Self(REG_RDI)
    }
    #[allow(unused)]
    pub const fn r10() -> Self {
        Self(REG_R10)
    }
    #[allow(unused)]
    pub const fn r11() -> Self {
        Self(REG_R11)
    }
    #[allow(unused)]
    pub const fn r12() -> Self {
        Self(REG_R12)
    }

    #[allow(unused)]
    pub const fn any() -> RegSet {
        RegSet(REG_ANY_GP)
    }
    #[allow(unused)]
    pub const fn none() -> RegSet {
        RegSet(0)
    }

    pub fn n(&self, size: u32) -> &'static str {
        match size {
            1 => self.n1(),
            4 => self.n4(),
            8 => self.n8(),
            _ => unreachable!(),
        }
    }

    pub fn n8(&self) -> &'static str {
        match self.0 {
            REG_RAX => "%rax",
            REG_RBX => "%rbx",
            REG_RCX => "%rcx",
            REG_RDX => "%rdx",
            REG_RSI => "%rsi",
            REG_RDI => "%rdi",
            REG_R10 => "%r10",
            REG_R11 => "%r11",
            REG_R12 => "%r12",

            REG_RSP => "%rsp",
            REG_RBP => "%rbp",
            _ => unreachable!(),
        }
    }

    pub fn n4(&self) -> &'static str {
        match self.0 {
            REG_RAX => "%eax",
            REG_RBX => "%ebx",
            REG_RCX => "%ecx",
            REG_RDX => "%edx",
            REG_RSI => "%esi",
            REG_RDI => "%edi",
            REG_R10 => "%r10d",
            REG_R11 => "%r11d",
            REG_R12 => "%r12d",

            REG_RSP => "%esp",
            REG_RBP => "%ebp",
            _ => unreachable!(),
        }
    }

    pub fn n1(&self) -> &'static str {
        match self.0 {
            REG_RAX => "%al",
            REG_RBX => "%bl",
            REG_RCX => "%cl",
            REG_RDX => "%dl",
            REG_RSI => "%sil",
            REG_RDI => "%dil",
            REG_R10 => "%r10b",
            REG_R11 => "%r11b",
            REG_R12 => "%r12b",

            REG_RSP => "%spl",
            REG_RBP => "%bpl",
            _ => unreachable!(),
        }
    }

    fn mask(&self) -> u32 {
        1 << self.0
    }
}

impl RegSet {
    fn new() -> Self {
        Self(0)
    }

    fn add(&mut self, reg: Reg) {
        self.0 |= reg.mask()
    }

    fn remove(&mut self, reg: Reg) {
        self.0 &= !reg.mask()
    }

    fn clear(&mut self) {
        self.0 = 0;
    }

    fn has(&self, reg: Reg) -> bool {
        (self.0 & reg.mask()) != 0
    }

    fn is_empty(&self) -> bool {
        self.0 == 0
    }

    fn select_any(&self) -> Reg {
        assert!(self.0 != 0);
        Reg(self.0.trailing_zeros())
    }
}

impl std::fmt::Debug for RegSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Regset({:10b})", self.0)
    }
}

impl From<Reg> for RegSet {
    fn from(value: Reg) -> Self {
        Self(value.mask())
    }
}

impl std::ops::Add<Reg> for Reg {
    type Output = RegSet;

    fn add(self, rhs: Reg) -> Self::Output {
        RegSet(self.mask() | rhs.mask())
    }
}

impl std::ops::Sub<RegSet> for RegSet {
    type Output = RegSet;

    fn sub(self, rhs: RegSet) -> Self::Output {
        Self(self.0 & !rhs.0)
    }
}

impl std::ops::Sub<Reg> for RegSet {
    type Output = RegSet;

    fn sub(self, rhs: Reg) -> Self::Output {
        Self(self.0 & !rhs.mask())
    }
}

impl std::ops::Add<Reg> for RegSet {
    type Output = RegSet;

    fn add(self, rhs: Reg) -> Self::Output {
        Self(self.0 | rhs.mask())
    }
}

pub(super) struct RegisterState {
    pin: RegSet,
    live: RegSet,
    tmp: RegSet,
    transaction: [u8; NUM_GP_REGS as usize],
    vreg_map: BTreeMap<VReg, Reg>,
    live_map: [VReg; NUM_GP_REGS as usize],
}

impl RegisterState {
    pub fn new() -> Self {
        Self {
            pin: RegSet::new(),
            live: RegSet::new(),
            tmp: RegSet::new(),
            transaction: [0; NUM_GP_REGS as usize],
            vreg_map: BTreeMap::new(),
            live_map: [VReg::discard(); NUM_GP_REGS as usize],
        }
    }

    #[allow(unused)]
    pub fn dump(&self) {
        println!("RegisterState");
        print!("  pin: ");
        for i in 0..NUM_GP_REGS {
            if self.pin.has(Reg(i)) {
                print!("{} ", Reg(i).n8());
            }
        }
        print!("\n  live: ");
        for i in 0..NUM_GP_REGS {
            if self.live.has(Reg(i)) {
                print!("{} ", Reg(i).n8());
            }
        }
        println!("\n  live_map:");
        for i in 0..NUM_GP_REGS {
            if self.live.has(Reg(i)) {
                println!("    {} => {:?}", Reg(i).n8(), self.live_map[i as usize]);
            }
        }
        println!("  vreg_map:");
        for (key, value) in self.vreg_map.iter() {
            println!("    {:?} => {}", key, value.n8());
        }
    }

    pub fn has_live_regs(&self) -> bool {
        !self.live.is_empty()
    }

    pub fn is_in_use(&self, reg: Reg) -> bool {
        self.live.has(reg)
    }

    pub fn peek(&mut self, vreg: VReg) -> Reg {
        *self.vreg_map.get(&vreg).unwrap()
    }

    fn add(&mut self, reg: Reg, vreg: VReg) {
        self.live.add(reg);
        self.live_map[reg.0 as usize] = vreg;
        assert!(self.vreg_map.insert(vreg, reg).is_none());
    }

    pub fn kill_vreg(&mut self, vreg: VReg) {
        let reg = self.vreg_map.remove(&vreg).unwrap();

        self.live.remove(reg);
        self.pin.remove(reg);
    }

    fn select_reg(&mut self, choices: RegSet) -> Reg {
        let choices = choices - self.pin;
        assert!(!choices.is_empty());

        let reg = choices.select_any();
        reg
    }

    fn pin(&mut self, reg: Reg) {
        self.pin.add(reg)
    }

    fn choose_any(&mut self, vreg: VReg, choices: RegSet) -> Option<Reg> {
        let choices = choices - self.live;
        if choices.is_empty() {
            None
        } else {
            let reg = choices.select_any();
            self.add(reg, vreg);
            Some(reg)
        }
    }

    fn mov(&mut self, src: Reg, dst: Reg) {
        assert!(self.live.has(src));
        assert!(!self.live.has(dst));
        assert!(!self.pin.has(src));

        let vreg = self.live_map[src.0 as usize];

        *self.vreg_map.get_mut(&vreg).unwrap() = dst;
        self.live_map[dst.0 as usize] = vreg;
        self.live.add(dst);
        self.live.remove(src);
    }

    fn implant(&mut self, vreg: VReg, at: Reg) -> Reg {
        assert!(self.live.has(at));
        assert!(!self.pin.has(at));

        let old = self.live_map[at.0 as usize];
        self.kill_vreg(old);

        self.add(at, vreg);
        self.choose_any(old, Reg::any()).unwrap()
    }

    fn swap(&mut self, a: Reg, b: Reg) {
        assert!(self.live.has(a) && self.live.has(b));
        assert!(!self.pin.has(a) && !self.pin.has(b));
        assert!(a != b);

        let vreg_a = self.live_map[a.0 as usize];
        let vreg_b = self.live_map[b.0 as usize];

        assert!(self.vreg_map.insert(vreg_a, b) == Some(a));
        assert!(self.vreg_map.insert(vreg_b, a) == Some(b));

        self.live_map[a.0 as usize] = vreg_b;
        self.live_map[b.0 as usize] = vreg_a;
    }

    fn relabel(&mut self, old: VReg, new: VReg) {
        let i = self.vreg_map.remove(&old).unwrap();

        self.live_map[i.0 as usize] = new;
        self.vreg_map.insert(new, i);
    }

    fn check_transaction(&mut self, requires: RegSet) {
        let mut mx = 0;
        let mut options = 0;
        for i in 0..NUM_GP_REGS {
            if requires.has(Reg(i)) {
                options += 1;

                mx = mx.max(self.transaction[i as usize]);
                self.transaction[i as usize] += 1;
            }
        }

        if mx >= options {
            panic!("register state miss-use: This register allocation might conflict with a more general allocation made earlier!");
        }
    }

    fn fixup_regs(
        &mut self,
        mut out: impl std::io::Write,
        is: (VReg, Reg),
        requires: RegSet,
    ) -> Result<Reg, std::io::Error> {
        let victim = self.select_reg(requires);

        if self.is_in_use(victim) {
            self.swap(victim, is.1);
            write!(out, "    xchgq {}, {}\n", victim.n8(), is.1.n8())?;
        } else {
            self.mov(is.1, victim);
            write!(out, "    movq {}, {}\n", is.1.n8(), victim.n8())?;
        }

        Ok(victim)
    }

    pub fn alloc_reg(
        &mut self,
        mut out: impl std::io::Write,
        vreg: VReg,
        requires: impl Into<RegSet>,
    ) -> Result<Reg, std::io::Error> {
        let requires = requires.into();
        self.check_transaction(requires);

        if let Some(reg) = self.choose_any(vreg, requires) {
            self.pin(reg);
            Ok(reg)
        } else {
            let reg = self.select_reg(requires);
            let replaced = self.implant(vreg, reg);
            write!(out, "    movq {}, {}\n", reg.n8(), replaced.n8())?;

            self.pin(reg);
            Ok(reg)
        }
    }

    pub fn alloc_tmp_reg(
        &mut self,
        out: impl std::io::Write,
        requires: impl Into<RegSet>,
    ) -> Result<Reg, std::io::Error> {
        let requires = requires.into();

        let tmp = VReg::next();
        let reg = self.alloc_reg(out, tmp, requires)?;
        self.tmp.add(reg);
        Ok(reg)
    }

    pub fn use_reg(
        &mut self,
        out: impl std::io::Write,
        vreg: VReg,
        requires: impl Into<RegSet>,
    ) -> Result<Reg, std::io::Error> {
        let requires = requires.into();
        self.check_transaction(requires);

        let mut reg = self.peek(vreg);
        if !requires.has(reg) {
            reg = self.fixup_regs(out, (vreg, reg), requires)?;
        }

        self.pin(reg);
        self.tmp.add(reg);
        Ok(reg)
    }

    pub fn reuse_reg(
        &mut self,
        out: impl std::io::Write,
        vreg: VReg,
        new: VReg,
        requires: impl Into<RegSet>,
    ) -> Result<Reg, std::io::Error> {
        let requires = requires.into();
        self.check_transaction(requires);

        let mut reg = self.peek(vreg);
        if !requires.has(reg) {
            reg = self.fixup_regs(out, (vreg, reg), requires)?;
        }

        self.relabel(vreg, new);
        self.pin(reg);
        Ok(reg)
    }

    pub fn spill(
        &mut self,
        mut out: impl std::io::Write,
        addr: Reg,
        ignore: impl Into<RegSet>,
    ) -> Result<(), std::io::Error> {
        let ignore = ignore.into();

        for i in 0..NUM_GP_REGS {
            let reg = Reg(i);

            if self.live.has(reg) && !ignore.has(reg) {
                write!(out, "    movq {}, {}({})\n", reg.n8(), i * 8, addr.n8())?;
            }
        }

        Ok(())
    }

    pub fn recover(
        &mut self,
        mut out: impl std::io::Write,
        addr: Reg,
        ignore: impl Into<RegSet>,
    ) -> Result<(), std::io::Error> {
        let ignore = ignore.into();

        for i in 0..NUM_GP_REGS {
            let reg = Reg(i);

            if self.live.has(reg) && !ignore.has(reg) {
                write!(out, "    movq {}({}), {}\n", i * 8, addr.n8(), reg.n8())?;
            }
        }

        Ok(())
    }

    pub fn release_transaction(&mut self) {
        self.transaction.fill(0);

        for i in 0..NUM_GP_REGS {
            if self.tmp.has(Reg(i)) {
                let vreg = self.live_map[i as usize];
                self.kill_vreg(vreg);
            }
        }

        self.pin.clear();
        self.tmp.clear();
    }
}
