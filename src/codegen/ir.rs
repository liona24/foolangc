use std::cell::OnceCell;
use std::collections::BTreeMap;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};
use std::sync::atomic::AtomicU32;

use super::allocator::VariableAllocator;
use crate::codegen::allocator::align;
use crate::parse_types::{Token, TokenId};
use crate::string_pool::StringPool;
use crate::tree_simplification::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeId {
    Void,
    BuiltinU8,
    BuiltinS64,
    UserDefined { name: Name },
}

#[derive(Debug)]
pub enum TypeCombineResult {
    Ok(Rc<Type>),
    CastA(Rc<Type>),
    CastB(Rc<Type>),
    Err,
}

#[derive(Debug)]
pub enum TypeVariant {
    Empty,
    Deferred(std::cell::OnceCell<Weak<Type>>),
    Number {
        size: u32,
        is_signed: bool,
    },
    Blob {
        size: u32,
        members: BTreeMap<Name, (u32, Weak<Type>)>,
    },
    Reference(Weak<Type>),
    Pointer(Weak<Type>),
    Array {
        count: u32,
        element_type: Weak<Type>,
    },
    Function {
        parameters: Vec<(Name, Weak<Type>)>,
        return_type: Weak<Type>,
    },
}

impl TypeId {
    fn pretty(&self, strings: &StringPool) -> String {
        match self {
            Self::Void => "void".to_string(),
            Self::BuiltinU8 => "byte".to_string(),
            Self::BuiltinS64 => "int".to_string(),
            Self::UserDefined { name } => name.pretty(strings),
        }
    }
}

impl TypeVariant {
    fn total_size(&self) -> u32 {
        match &self {
            Self::Number { size, .. } | Self::Blob { size, .. } => *size,
            Self::Pointer(_) => 8,
            Self::Array {
                count,
                element_type,
            } => count * element_type.upgrade().unwrap().size(),
            Self::Reference(to) => to.upgrade().unwrap().size(),
            Self::Empty | Self::Function { .. } => 0,
            Self::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().size(),
        }
    }

    fn count(&self) -> u32 {
        match &self {
            Self::Empty | Self::Function { .. } => 0,
            Self::Number { .. } | Self::Blob { .. } | Self::Pointer(_) => 1,
            Self::Array { count, .. } => *count,
            Self::Reference(to) => to.upgrade().unwrap().t.count(),
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().t.count(),
        }
    }
}

pub struct Blob {
    t: Rc<Type>,
}

pub struct Array {
    t: Rc<Type>,
}

pub struct Function {
    t: Rc<Type>,
}

impl Function {
    pub fn name(&self) -> Name {
        self.t.name().unwrap()
    }

    pub fn arg_size(&self) -> u32 {
        if let TypeVariant::Function { parameters, .. } = &self.t.t {
            parameters
                .iter()
                .map(|(_, p)| p.upgrade().unwrap().size())
                .sum()
        } else {
            unreachable!()
        }
    }

    pub fn parameters(&self) -> &[(Name, Weak<Type>)] {
        if let TypeVariant::Function { parameters, .. } = &self.t.t {
            &parameters
        } else {
            unreachable!()
        }
    }

    pub fn return_type(&self) -> Rc<Type> {
        if let TypeVariant::Function { return_type, .. } = &self.t.t {
            return_type.upgrade().unwrap()
        } else {
            unreachable!()
        }
    }
}

impl Blob {
    pub fn size(&self) -> u32 {
        if let TypeVariant::Blob { size, .. } = &self.t.t {
            *size
        } else {
            unreachable!()
        }
    }

    pub fn members(&self) -> &BTreeMap<Name, (u32, Weak<Type>)> {
        if let TypeVariant::Blob { members, .. } = &self.t.t {
            members
        } else {
            unreachable!()
        }
    }
}

impl Array {
    pub fn count(&self) -> u32 {
        if let TypeVariant::Array { count, .. } = &self.t.t {
            *count
        } else {
            unreachable!()
        }
    }

    pub fn element_type(&self) -> Rc<Type> {
        if let TypeVariant::Array { element_type, .. } = &self.t.t {
            element_type.upgrade().unwrap()
        } else {
            unreachable!()
        }
    }
}

pub struct Type {
    pub id: TypeId,
    pub t: TypeVariant,
    this: Weak<Type>,
}

impl Type {
    pub fn rc(&self) -> Rc<Type> {
        self.this.upgrade().unwrap()
    }
    pub fn size(&self) -> u32 {
        self.t.total_size()
    }

    pub fn name(&self) -> Option<Name> {
        match self.id {
            TypeId::UserDefined { name } => Some(name),
            _ => None,
        }
    }

    pub fn real_id(&self) -> TypeId {
        match &self.t {
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().real_id(),
            TypeVariant::Reference(to) => to.upgrade().unwrap().real_id(),
            _ => self.id,
        }
    }

    fn is_same_as(&self, id: Option<TypeId>, v: &TypeVariant) -> bool {
        match (&self.t, v) {
            (TypeVariant::Number { .. }, TypeVariant::Number { .. })
            | (TypeVariant::Blob { .. }, TypeVariant::Blob { .. }) => Some(self.id) == id,
            (TypeVariant::Reference(a), TypeVariant::Reference(b))
            | (TypeVariant::Pointer(a), TypeVariant::Pointer(b)) => a.ptr_eq(b),
            (
                TypeVariant::Array {
                    count: count_a,
                    element_type: a,
                },
                TypeVariant::Array {
                    count: count_b,
                    element_type: b,
                },
            ) => *count_a == *count_b && Rc::ptr_eq(&a.upgrade().unwrap(), &b.upgrade().unwrap()),

            (TypeVariant::Function { .. }, _) => {
                // functions should never be equal. we only differentiate by name.
                false
            }

            _ => false,
        }
    }

    pub fn as_array(&self) -> Option<Array> {
        match &self.t {
            TypeVariant::Array { .. } => Some(Array {
                t: self.this.upgrade().unwrap(),
            }),
            TypeVariant::Pointer(pointee) => pointee.upgrade().unwrap().as_array(),
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().as_array(),
            _ => None,
        }
    }

    pub fn as_blob(&self) -> Option<Blob> {
        match &self.t {
            TypeVariant::Blob { .. } => Some(Blob {
                t: self.this.upgrade().unwrap(),
            }),
            TypeVariant::Pointer(pointee) => pointee.upgrade().unwrap().as_blob(),
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().as_blob(),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<Function> {
        match &self.t {
            TypeVariant::Function { .. } => Some(Function {
                t: self.this.upgrade().unwrap(),
            }),
            TypeVariant::Pointer(pointee) => pointee.upgrade().unwrap().as_function(),
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().as_function(),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<Rc<Type>> {
        match &self.t {
            TypeVariant::Pointer(pointee) => Some(pointee.upgrade().unwrap()),
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().as_pointer(),
            _ => None,
        }
    }

    fn order(&self) -> u64 {
        match &self.t {
            TypeVariant::Empty => u64::MAX,
            TypeVariant::Number { size, .. } => (*size as u64) << 31,
            TypeVariant::Blob { size, .. } => (*size as u64) << 31,
            TypeVariant::Reference(to) => to.upgrade().unwrap().order(),
            TypeVariant::Pointer(_) => 8 << 31,
            TypeVariant::Array { .. } => (self.size() as u64) << 31,
            TypeVariant::Function { .. } => 0,
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().order(),
        }
    }

    pub fn combine_types(a: &Self, b: &Self) -> TypeCombineResult {
        if a.real_id() == b.real_id() {
            TypeCombineResult::Ok(a.this.upgrade().unwrap())
        } else if !a.is_ref() && a.is_value_type() && a.order() >= b.order() {
            TypeCombineResult::CastB(a.rc())
        } else if !b.is_ref() && b.is_value_type() && b.order() >= a.order() {
            TypeCombineResult::CastA(b.rc())
        } else {
            TypeCombineResult::Err
        }
    }

    pub fn assign(&self, value: &Self) -> TypeCombineResult {
        assert!(self.is_ref());

        let t = self.as_deref();
        if t.id == value.id {
            TypeCombineResult::Ok(t)
        } else if t.order() >= value.order() {
            TypeCombineResult::CastB(t)
        } else {
            TypeCombineResult::Err
        }
    }

    pub fn deref_ptr_or_ref(&self) -> Option<Rc<Type>> {
        match &self.t {
            TypeVariant::Pointer(pointee) | TypeVariant::Reference(pointee) => pointee.upgrade(),
            _ => None,
        }
    }

    pub fn is_ref(&self) -> bool {
        if let TypeVariant::Reference(_) = &self.t {
            true
        } else {
            false
        }
    }

    pub fn as_deref(&self) -> Rc<Type> {
        match &self.t {
            TypeVariant::Reference(to) => to.upgrade().unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn is_value_ref(&self) -> bool {
        if let TypeVariant::Reference(to) = &self.t {
            let to = to.upgrade().unwrap();
            to.is_value_type()
        } else {
            false
        }
    }

    pub fn is_value_type(&self) -> bool {
        match &self.t {
            TypeVariant::Number { .. } | TypeVariant::Pointer(_) => true,
            TypeVariant::Empty
            | TypeVariant::Blob { .. }
            | TypeVariant::Reference(_)
            | TypeVariant::Array { .. }
            | TypeVariant::Function { .. } => false,
            TypeVariant::Deferred(inner) => inner.get().unwrap().upgrade().unwrap().is_value_type(),
        }
    }

    pub fn is_signed(&self) -> bool {
        match &self.t {
            TypeVariant::Number { is_signed, .. } => *is_signed,
            TypeVariant::Reference(to) => to.upgrade().unwrap().is_signed(),
            _ => false,
        }
    }

    fn pretty_internal(&self, name: String, strings: &StringPool) -> String {
        match &self.t {
            TypeVariant::Empty | TypeVariant::Number { .. } => name,
            TypeVariant::Blob { .. } => format!("struct {}", name),
            TypeVariant::Reference(to) => to.upgrade().unwrap().pretty(strings),
            TypeVariant::Pointer(to) => format!("{}*", to.upgrade().unwrap().pretty(strings)),
            TypeVariant::Array {
                count,
                element_type,
            } => format!(
                "{}[{}]",
                element_type.upgrade().unwrap().pretty(strings),
                count
            ),
            TypeVariant::Function {
                parameters,
                return_type,
            } => {
                let mut parts = vec![format!("func {}(", name)];
                for (name, t) in parameters.iter() {
                    parts.push(format!(
                        "{} {}",
                        t.upgrade().unwrap().pretty(strings),
                        name.pretty(strings)
                    ));
                }
                format!(
                    "{}) = {}",
                    parts.join(", "),
                    return_type.upgrade().unwrap().pretty(strings)
                )
            }
            TypeVariant::Deferred(inner) => inner
                .get()
                .unwrap()
                .upgrade()
                .unwrap()
                .pretty_internal(name, strings),
        }
    }

    pub fn pretty(&self, strings: &StringPool) -> String {
        let name = self.id.pretty(strings);
        self.pretty_internal(name, strings)
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type{{")?;

        let mut x = self.this.upgrade().unwrap();
        loop {
            match x.id {
                TypeId::Void => write!(f, "void")?,
                TypeId::BuiltinU8 => write!(f, "u8")?,
                TypeId::BuiltinS64 => write!(f, "s64")?,
                TypeId::UserDefined { .. } => {}
            }

            match &x.t {
                TypeVariant::Empty => {}
                TypeVariant::Number { size, is_signed } => write!(f, "[{}, {}]", size, is_signed)?,
                TypeVariant::Blob { size, .. } => write!(f, "Blob[{}]", size)?,
                TypeVariant::Reference(to) => {
                    write!(f, "-ref-> ")?;
                    x = to.upgrade().unwrap();
                    continue;
                }
                TypeVariant::Pointer(to) => {
                    write!(f, "-ptr-> ")?;
                    x = to.upgrade().unwrap();
                    continue;
                }
                TypeVariant::Array {
                    count,
                    element_type,
                } => {
                    write!(
                        f,
                        "Array[{} x {:?}]",
                        count,
                        element_type.upgrade().unwrap()
                    )?;
                }
                TypeVariant::Function { .. } => write!(f, "Func")?,
                TypeVariant::Deferred(inner) => {
                    if let Some(inner) = inner.get() {
                        write!(f, "{:?}", inner.upgrade().unwrap())?;
                    } else {
                        write!(f, "<deferred>")?;
                    }
                }
            }

            break;
        }

        write!(f, "}}")
    }
}

pub struct TypeContainer {
    id_lookup: BTreeMap<TypeId, Rc<Type>>,
    type_lookup: BTreeMap<(u32, u32), Vec<Rc<Type>>>,
    deferred: BTreeMap<TypeId, Rc<Type>>,
}

impl TypeContainer {
    pub fn new(strings: &mut StringPool) -> Self {
        let mut rv = Self {
            id_lookup: BTreeMap::new(),
            type_lookup: BTreeMap::new(),
            deferred: BTreeMap::new(),
        };

        rv.find_or_insert_type(
            Some(TypeId::BuiltinU8),
            TypeVariant::Number {
                size: 1,
                is_signed: false,
            },
        );
        rv.find_or_insert_type(
            Some(TypeId::BuiltinS64),
            TypeVariant::Number {
                size: 8,
                is_signed: true,
            },
        );
        rv.find_or_insert_type(Some(TypeId::Void), TypeVariant::Empty);

        macro_rules! name {
            ($s:expr) => {
                Name::Builtin(strings.insert_str($s))
            };
        }

        let arg_name = name!("arg");

        rv.insert_function(
            name!("exit"),
            [(arg_name, TypeId::BuiltinS64)].into_iter(),
            TypeId::Void,
        );
        rv.insert_function(
            name!("syscall"),
            [(arg_name, TypeId::BuiltinS64); 7].into_iter(),
            TypeId::BuiltinS64,
        );

        rv
    }

    fn find_or_insert_type(&mut self, id_hint: Option<TypeId>, variant: TypeVariant) -> Rc<Type> {
        if let Some(id) = id_hint {
            if let Some(deferred) = self.deferred.get(&id).cloned() {
                let t = self.find_or_insert_type(None, variant);
                if let TypeVariant::Deferred(inner) = &deferred.t {
                    inner.set(Rc::downgrade(&t)).unwrap();
                } else {
                    unreachable!()
                }
                return t;
            }

            if let Some(t) = self.id_lookup.get(&id) {
                todo!("type is shadowed!");
                return t.clone();
            }
        }

        // TODO: this "hashing" heuristic is a little naive and will probably not
        // scale too well. Not sure what else we can do.
        let size = variant.total_size();
        let count = variant.count();

        let types = self.type_lookup.entry((size, count)).or_default();
        if let Some(t) = types.iter().find(|t| t.is_same_as(id_hint, &variant)) {
            t.clone()
        } else {
            let id = id_hint.unwrap_or_else(|| TypeId::UserDefined {
                name: Name::new_internal(),
            });

            let t = Rc::new_cyclic(|this| Type {
                id,
                t: variant,
                this: this.clone(),
            });
            types.push(t.clone());
            self.id_lookup.insert(id, t.clone());

            t
        }
    }

    pub fn insert_function(
        &mut self,
        name: Name,
        parameters: impl Iterator<Item = (Name, TypeId)>,
        return_type: TypeId,
    ) -> Rc<Type> {
        let return_type = Rc::downgrade(&self.get_type(return_type).unwrap());
        let parameters = parameters
            .map(|(n, t)| (n, Rc::downgrade(&self.get_type(t).unwrap())))
            .collect();

        self.find_or_insert_type(
            Some(TypeId::UserDefined { name }),
            TypeVariant::Function {
                parameters,
                return_type,
            },
        )
    }

    pub fn get_type(&self, id: TypeId) -> Option<Rc<Type>> {
        self.id_lookup
            .get(&id)
            .or_else(|| self.deferred.get(&id))
            .cloned()
    }

    pub fn get_function(&self, name: Name) -> Option<Function> {
        self.get_type(TypeId::UserDefined { name })
            .map(|t| t.as_function())
            .flatten()
    }

    pub fn insert_pointer(&mut self, to: TypeId) -> Rc<Type> {
        let pointee = self.get_type(to).unwrap();
        self.find_or_insert_type(None, TypeVariant::Pointer(Rc::downgrade(&pointee)))
    }

    pub fn insert_ref(&mut self, to: TypeId) -> Rc<Type> {
        let pointee = self.get_type(to).unwrap();
        self.find_or_insert_type(None, TypeVariant::Reference(Rc::downgrade(&pointee)))
    }

    pub fn hint_future_type(&mut self, name: Name) -> TypeId {
        let id = TypeId::UserDefined { name };
        let t = Rc::new_cyclic(|this| Type {
            id,
            t: TypeVariant::Deferred(OnceCell::new()),
            this: this.clone(),
        });

        self.deferred.insert(id, t);
        id
    }

    pub fn insert_blob(
        &mut self,
        name: Name,
        members: impl IntoIterator<Item = (Name, TypeId)>,
    ) -> Rc<Type> {
        let mut tmp = BTreeMap::new();
        let mut offset = 0;

        for (name, typeid) in members.into_iter() {
            let t = self.get_type(typeid).unwrap();
            let size = t.size();
            tmp.insert(name, (offset, Rc::downgrade(&t)));

            offset += align(size);
        }

        self.find_or_insert_type(
            Some(TypeId::UserDefined { name }),
            TypeVariant::Blob {
                size: offset,
                members: tmp,
            },
        )
    }
    pub fn insert_array(&mut self, of: TypeId, count: u32) -> Rc<Type> {
        let of = self.get_type(of).unwrap();
        self.find_or_insert_type(
            None,
            TypeVariant::Array {
                count,
                element_type: Rc::downgrade(&of),
            },
        )
    }
}

pub struct Variable {
    pub addr: Address,
    pub typ: Rc<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Name {
    User(u32),
    Builtin(u32),
    Internal(u32),
    EndUser(u32),
    EndInternal(u32),
    EndBuiltin(u32),
}

static NEXT_LABEL: AtomicU32 = AtomicU32::new(1);

impl Name {
    pub fn to_label(&self, strings: &StringPool) -> String {
        match self {
            Name::User(n) => format!("__{}", strings.get(*n)),
            Name::Internal(n) => format!(".Ltmp{}", n),
            Name::Builtin(n) => format!("{}", strings.get(*n)),
            Name::EndUser(n) => format!(".Lend__{}", strings.get(*n)),
            Name::EndInternal(n) => format!(".Lend_{}", n),
            Name::EndBuiltin(n) => format!(".Lend_{}", strings.get(*n)),
        }
    }

    pub fn new_internal() -> Self {
        Self::Internal(NEXT_LABEL.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }

    pub fn end(self) -> Self {
        match self {
            Self::User(n) => Self::EndUser(n),
            Self::Builtin(n) => Self::EndBuiltin(n),
            Self::Internal(n) => Self::EndInternal(n),
            Self::EndUser(_) | Self::EndInternal(_) | Self::EndBuiltin(_) => self,
        }
    }

    pub fn pretty(&self, strings: &StringPool) -> String {
        match self {
            Self::Builtin(id) => format!("@{}", strings.get(*id)),
            Self::User(id) => strings.get(*id).to_string(),
            _ => "".to_string(),
        }
    }
}

impl<T: AsRef<Token>> From<T> for Name {
    fn from(value: T) -> Self {
        let r = value.as_ref();
        match r.id {
            TokenId::Identifier => Self::User(r.meta as u32),
            TokenId::BuiltinIdentifier => Self::Builtin(r.meta as u32),

            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Address {
    Global { name: Name },
    Local(u32),
    Argument(u32),
}

#[derive(Clone, Copy)]
pub struct VReg {
    id: u32,
    bitfield: u32,
}

static NEXT_VREG: AtomicU32 = AtomicU32::new(1);

impl VReg {
    pub fn next() -> Self {
        Self {
            id: NEXT_VREG.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            bitfield: 0,
        }
    }

    pub fn discard() -> Self {
        Self { id: 0, bitfield: 0 }
    }

    pub fn has_info(&self) -> bool {
        self.bitfield != 0
    }

    pub fn add_info(&mut self, size: u32, is_ref: bool, is_signed: bool) {
        let is_ref = if is_ref { 0b10 } else { 0b00 };
        let is_signed = if is_signed { 0b01 } else { 0b00 };
        self.bitfield = (size << 2) | is_ref | is_signed;
    }

    pub fn ref_to_ptr(self) -> Self {
        assert!(self.is_ref());

        let size = 8;
        Self {
            id: self.id,
            bitfield: size << 2,
        }
    }

    pub fn is_signed(&self) -> bool {
        (self.bitfield & 0b01) != 0
    }

    pub fn is_ref(&self) -> bool {
        (self.bitfield & 0b10) != 0
    }

    pub fn is_value(&self) -> bool {
        !self.is_ref()
    }

    pub fn size(&self) -> u32 {
        if self.is_ref() {
            8
        } else {
            self.bitfield >> 2
        }
    }

    pub fn deref_size(&self) -> u32 {
        assert!(self.is_ref());
        self.bitfield >> 2
    }
}

impl std::fmt::Debug for VReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.id == 0 {
            write!(f, "VReg(0)")
        } else {
            write!(f, "VReg({}", self.id)?;

            if self.is_signed() {
                write!(f, " signed")?;
            }

            if self.is_ref() {
                write!(f, " ref [*{}]", self.deref_size())?;
            } else {
                write!(f, " [{}]", self.size())?;
            }

            write!(f, ")")
        }
    }
}

impl PartialEq for VReg {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for VReg {}

impl Ord for VReg {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}
impl PartialOrd for VReg {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

#[derive(Debug)]
pub struct IrStatement<'t> {
    pub result: VReg,
    pub typ: Rc<Type>,
    pub op: IrOp,
    _lifetime: PhantomData<&'t ()>,
}

impl<'t> IrStatement<'t> {
    pub fn new(result: VReg, typ: Rc<Type>, op: IrOp) -> Self {
        Self {
            result,
            typ,
            op,
            _lifetime: PhantomData,
        }
    }
}

pub struct Ir<'t> {
    pub ir: Vec<IrStatement<'t>>,
    pub constants: BTreeMap<u32, Name>,
}

#[derive(Debug)]
pub enum IrOp {
    None,
    LoadConst {
        value: u64,
    },
    Unary {
        op: UnaryOp,
        operand: VReg,
    },
    Extend {
        operand: VReg,
    },
    Binary {
        op: BinaryOp,
        left: VReg,
        right: VReg,
    },
    LoadAddr {
        addr: Address,
    },
    MemAssign {
        src: VReg,
        dst: VReg,
    },
    MemLoad {
        mem_src: VReg,
    },
    JmpLabel {
        name: Name,
    },
    Label(Name),
    ProvideArg(VReg),
    Call {
        func: Name,
        arg_size: u32,
    },
    Return {
        to: Name,
        value: VReg,
    },
    BranchCond {
        cond: VReg,
        true_branch: Name,
        false_branch: Name,
    },
    /// We use this pseudo op to not do a liveness analysis of the registers.
    /// Basically this op means that the contained reg is dead now
    Kill(VReg),

    Nop(VReg),

    EnterScopePlaceholder,
    EnterScope(VariableAllocator),
    LeaveScope(Name),
}
