use std::collections::{BTreeMap, HashMap};

pub struct StringPool {
    strings: HashMap<u32, &'static str>,
    custom: BTreeMap<&'static str, u32>,

    null: &'static str,
}

impl StringPool {
    pub fn new() -> &'static mut Self {
        Box::leak(Box::new(Self {
            strings: HashMap::new(),

            custom: BTreeMap::new(),

            null: "",
        }))
    }

    fn next_id(&self) -> u32 {
        self.strings.len() as u32 + 1
    }

    pub fn insert_str(&mut self, s: &str) -> u32 {
        if let Some(&id) = self.custom.get(s) {
            id
        } else {
            let r = Box::leak(Box::new(s.to_owned()));

            let id = self.next_id();
            self.strings.insert(id, r);
            self.custom.insert(r, id);

            id
        }
    }

    pub fn get(&self, id: u32) -> &str {
        self.strings.get(&id).unwrap_or(&self.null)
    }
}
