use crate::codegen::ir::{Address, Name};
use crate::codegen::register_state::NUM_GP_REGS;

#[derive(Debug)]
pub(super) enum VariableAllocatorImpl {
    /// A vec of (name, size) tuples
    Global {
        globals: Vec<(Name, u32)>,
    },
    Local {
        stack_size: u32,
        param_size: u32,
    },
}

pub fn align(n: u32) -> u32 {
    (n + 7) & !7
}

#[derive(Debug)]
pub struct VariableAllocator {
    pub name: Name,
    pub spill_space: Option<Address>,
    pub(super) allocator: VariableAllocatorImpl,
}

impl VariableAllocator {
    pub fn new_local(name: Name) -> Self {
        Self {
            name,
            spill_space: None,
            allocator: VariableAllocatorImpl::Local {
                stack_size: 0,
                param_size: 0,
            },
        }
    }
    pub fn new_global() -> Self {
        Self {
            name: Name::new_internal(),
            spill_space: None,
            allocator: VariableAllocatorImpl::Global {
                globals: Vec::new(),
            },
        }
    }

    pub fn allocate(&mut self, name: Name, total_size: u32) -> Address {
        match &mut self.allocator {
            VariableAllocatorImpl::Global { globals } => {
                globals.push((name, total_size));
                Address::Global { name }
            }
            VariableAllocatorImpl::Local { stack_size, .. } => {
                *stack_size += align(total_size);
                let offset = *stack_size;

                Address::Local(offset)
            }
        }
    }

    pub fn add_param(&mut self, total_size: u32) -> Address {
        match &mut self.allocator {
            VariableAllocatorImpl::Local { param_size, .. } => {
                let offset = *param_size;
                *param_size += align(total_size);

                /*
                *param_size -= total_size;
                let offset = *param_size;
                */

                Address::Argument(offset)
            }
            _ => unreachable!(),
        }
    }

    pub fn reserve_spill(&mut self) {
        if self.spill_space.is_none() {
            self.spill_space = Some(self.allocate(Name::new_internal(), NUM_GP_REGS as u32 * 8));
        }
    }
}
