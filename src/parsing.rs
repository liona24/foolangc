use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::pin::Pin;
use std::rc::Rc;

use crate::error::CompilerError;
use crate::parse_types::{NonTerminal, Symbol, Token, TokenId, TokenRange};
use crate::string_pool::StringPool;

pub trait Lexer {
    fn next_token(&mut self, strings: &mut StringPool) -> Result<Token, CompilerError>;
    fn get_input_hint(&self, range: &TokenRange, range_b: Option<&TokenRange>) -> String;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rule {
    pub left: NonTerminal,
    pub right: Vec<Symbol>,
}

pub struct Grammar {
    starting_rule: Rule,
    rules: BTreeMap<Symbol, Vec<Rule>>,
}

impl Rule {
    pub fn pretty(&self, pool: Option<&StringPool>) -> String {
        let mut parts = vec![format!("{:?} → [", self.left)];
        for ri in self.right.iter() {
            parts.push(ri.pretty(pool));
        }
        parts.push("]".to_string());

        parts.join(" ")
    }
}

impl Grammar {
    pub fn new(mut rules: BTreeMap<Symbol, Vec<Rule>>) -> Self {
        let starting_rule = rules
            .remove(&Symbol::NonTerminal(NonTerminal::Start))
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        Self {
            starting_rule,
            rules,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Item<'r> {
    rule: &'r Rule,
    pos_rule: usize,
    pos_input: usize,
}

impl<'r> Item<'r> {
    fn new(rule: &'r Rule, pos_rule: usize, pos_input: usize) -> Self {
        Self {
            rule,
            pos_rule,
            pos_input,
        }
    }

    fn is_finished(&self) -> bool {
        self.pos_rule >= self.rule.right.len()
    }

    fn next_symbol(&self) -> &Symbol {
        &self.rule.right[self.pos_rule]
    }

    fn advanced(&self) -> Self {
        Self {
            rule: self.rule,
            pos_rule: self.pos_rule + 1,
            pos_input: self.pos_input,
        }
    }

    fn label(&self) -> NodeLabel<'r> {
        if self.is_finished() {
            NodeLabel::Symbol(Symbol::NonTerminal(self.rule.left))
        } else {
            NodeLabel::Item(self.clone())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeLabel<'r> {
    Symbol(Symbol),
    Item(Item<'r>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Family<'r> {
    pub a: Rc<Node<'r>>,
    pub b: Option<Rc<Node<'r>>>,
}

struct FamilyIter<'r, 'f> {
    family: &'f Family<'r>,
    pos: usize,
}

impl<'r, 'f> Iterator for FamilyIter<'r, 'f> {
    type Item = &'f Rc<Node<'r>>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = match self.pos {
            0 => Some(&self.family.a),
            1 => self.family.b.as_ref(),
            _ => None,
        };

        self.pos += 1;
        res
    }
}

impl<'r> Family<'r> {
    fn len(&self) -> usize {
        if self.b.is_some() {
            2
        } else {
            1
        }
    }

    fn iter(&self) -> impl Iterator<Item = &Rc<Node<'r>>> + '_ {
        FamilyIter {
            family: self,
            pos: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node<'r> {
    pub label: NodeLabel<'r>,
    pub left_extend: usize,
    pub right_extend: usize,
}

impl<'r> Node<'r> {
    fn new(label: NodeLabel<'r>, left_extend: usize, right_extend: usize) -> Rc<Self> {
        Rc::new(Self {
            label,
            left_extend,
            right_extend,
        })
    }

    fn pretty(&self, pool: Option<&StringPool>) -> String {
        let label = match &self.label {
            NodeLabel::Symbol(s) => s.pretty(pool),
            NodeLabel::Item(i) => i.rule.pretty(pool),
        };
        format!(
            "({} {{ {} {} }})",
            label, self.left_extend, self.right_extend
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct State<'r> {
    item: Item<'r>,
    node: RefCell<Option<Rc<Node<'r>>>>,
}

impl<'r> State<'r> {
    fn new(item: Item<'r>, node: Option<Rc<Node<'r>>>) -> Self {
        Self {
            item,
            node: RefCell::new(node),
        }
    }

    fn is_finished(&self) -> bool {
        self.item.is_finished()
    }
}

struct ExpectedToken<'r> {
    token_id: TokenId,
    next_state: State<'r>,
}

impl<'r> ExpectedToken<'r> {
    fn new(token_id: TokenId, next_state: State<'r>) -> Self {
        Self {
            token_id,
            next_state,
        }
    }
}

struct RefSet<'a, T> {
    store: RefCell<BTreeSet<&'a T>>,
    linear: RefCell<Vec<Pin<Box<T>>>>,
}

impl<'a, T: Ord> RefSet<'a, T> {
    fn new() -> Self {
        Self {
            store: RefCell::new(BTreeSet::new()),
            linear: RefCell::new(Vec::new()),
        }
    }

    fn len(&self) -> usize {
        self.linear.borrow().len()
    }

    fn add(&self, obj: T) -> &T {
        let mut store = self.store.borrow_mut();

        if let Some(&t) = store.get(&obj) {
            t
        } else {
            let pin = Box::pin(obj);

            // this is safe because this is a non-deleting container and the reference does not
            // outlive the container and the backing store is pinned.
            let r = unsafe { std::mem::transmute::<&'_ T, &'_ T>(pin.as_ref().get_ref()) };

            store.insert(r);
            self.linear.borrow_mut().push(pin);
            r
        }
    }
}

impl<'a, T> std::ops::Index<usize> for RefSet<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        // this is safe because this is a non-deleting container and the reference does not
        // outlive the container and the backing store is pinned.
        unsafe {
            std::mem::transmute::<&'_ T, &'_ T>(self.linear.borrow()[index].as_ref().get_ref())
        }
    }
}

fn next_node<'r>(
    of: &Option<Rc<Node<'r>>>,
    item: &Item<'r>,
    pos_input: usize,
    prev_node: &Rc<Node<'r>>,
    v: &mut BTreeMap<Rc<Node<'r>>, Vec<Family<'r>>>,
) -> Option<Rc<Node<'r>>> {
    // implicit EPS check
    if !item.is_finished() && item.pos_rule == 1 {
        return Some(prev_node.clone());
    }

    let node = Node::new(item.label(), item.pos_input, pos_input);
    let families = v.entry(node.clone()).or_default();

    let a;
    let b;
    if let Some(of) = of {
        a = of;
        b = Some(prev_node);
    } else {
        a = prev_node;
        b = None;
    }

    if families
        .iter()
        .find(|fam| &fam.a == a && fam.b.as_ref() == b)
        .is_none()
    {
        families.push(Family {
            a: a.clone(),
            b: b.cloned(),
        });
    }

    Some(node)
}

pub struct ParseForest<'g> {
    pub families: BTreeMap<Rc<Node<'g>>, Vec<Family<'g>>>,
    pub root: Rc<Node<'g>>,
}

impl<'g> ParseForest<'g> {
    #[cfg(feature = "graphviz-rust")]
    #[allow(unused)]
    pub fn dump(&self, strings: &StringPool) -> String {
        use graphviz_rust::cmd::Format;
        use graphviz_rust::dot_generator::{attr, edge, graph, id, node, node_id, stmt};
        use graphviz_rust::dot_structures::*;
        use graphviz_rust::exec;
        use graphviz_rust::printer::PrinterContext;

        let mut g = graph!(strict di id!("parseforest"));

        let get_node_id = |n: &Rc<crate::parsing::Node<'g>>, i: usize| {
            let id = Rc::as_ptr(n) as usize;

            format!("n{}_{}", id, i)
        };

        let new_node = |id: &str, n: &Rc<crate::parsing::Node<'g>>| {
            let label = format!("\"{}\"", n.pretty(Some(strings)).escape_debug());

            node!(id;
                attr!("style","\"filled,rounded\""),
                attr!("fillcolor","\"#fafaf0\""),
                attr!("shape","box"),
                attr!("margin","\"0.5,0.25\""),
                attr!("label",label)
            )
        };
        let new_intermediate_node = |id: &str| {
            node!(
                id;
                attr!("style","\"filled,rounded\""),
                attr!("fillcolor","\"#fafaf0\""),
                attr!("shape","circle"),
                attr!("width",0.25),
                attr!("fixedsize",true),
                attr!("label", "\"\"")
            )
        };

        let mut parents: BTreeMap<String, Vec<String>> = BTreeMap::new();
        let mut visited = BTreeSet::new();
        let mut q = VecDeque::new();
        q.push_back(&self.root);

        while let Some(n) = q.pop_front() {
            if !visited.insert(n) {
                continue;
            }

            let n_id = get_node_id(n, 0);
            g.add_stmt(stmt!(new_node(&n_id, n)));

            let families: Vec<_> = self.families.get(n).into_iter().flatten().collect();

            if families.iter().map(|f| f.len()).sum::<usize>() > 1 {
                let mut intermediate = 1;

                for f in families {
                    let i_id = get_node_id(n, intermediate);

                    for child in f.iter() {
                        q.push_back(child);
                        parents
                            .entry(get_node_id(child, 0))
                            .or_default()
                            .push(i_id.clone());
                    }

                    g.add_stmt(stmt!(new_intermediate_node(&i_id)));
                    g.add_stmt(stmt!(edge!(node_id!(&n_id) => node_id!(&i_id))));
                    intermediate += 1;
                }
            } else if let Some(f) = families.first() {
                parents.entry(get_node_id(&f.a, 0)).or_default().push(n_id);
                q.push_back(&f.a);
            }
        }

        for (child, parents) in parents.into_iter() {
            for parent in parents {
                g.add_stmt(stmt!(edge!(node_id!(parent) => node_id!(child))));
            }
        }

        exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()]).unwrap()
    }
}

struct ParseForestIterator<'p, 'g> {
    parse_forest: &'p ParseForest<'g>,
    q: VecDeque<&'p Rc<Node<'g>>>,
    visited: BTreeSet<&'p Rc<Node<'g>>>,
}

impl<'p, 'g> Iterator for ParseForestIterator<'p, 'g> {
    type Item = &'p Rc<Node<'g>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(top) = self.q.pop_front() {
            if let Some(families) = self.parse_forest.families.get(top) {
                for fam in families {
                    for n in fam.iter() {
                        if self.visited.insert(n) {
                            self.q.push_back(n);
                        }
                    }
                }
            }

            Some(top)
        } else {
            None
        }
    }
}

pub fn parse<'g>(
    lexer: &mut impl Lexer,
    grammar: &'g Grammar,
    strings: &mut StringPool,
) -> Result<ParseForest<'g>, CompilerError> {
    let first_symbol =
        grammar.starting_rule.right.first().ok_or_else(|| {
            CompilerError::internal_error("invalid grammar: empty starting rule!")
        })?;
    let initial_state = State::new(Item::new(&grammar.starting_rule, 0, 0), None);

    let mut states = vec![RefSet::new()];

    let mut expected_tokens = Vec::new();

    match first_symbol {
        Symbol::NonTerminal(..) => {
            states[0].add(initial_state);
        }
        Symbol::Terminal { token } => {
            expected_tokens.push(ExpectedToken::new(token.id, initial_state));
        }
    }

    let mut i = 0;
    let mut h = BTreeMap::new();
    let mut v = BTreeMap::new();
    loop {
        let mut j = 0;
        while j < states[i].len() {
            let state = &states[i][j];

            if !state.is_finished() {
                for rule in grammar
                    .rules
                    .get(state.item.next_symbol())
                    .into_iter()
                    .flatten()
                {
                    let first_symbol = rule.right.get(0).ok_or_else(|| {
                        CompilerError::internal_error("invalid grammar: empty rule!")
                    })?;
                    let new_state = State::new(Item::new(rule, 0, i), None);

                    match first_symbol {
                        Symbol::NonTerminal(..) => {
                            states[i].add(new_state);
                        }
                        Symbol::Terminal { token } => {
                            expected_tokens.push(ExpectedToken::new(token.id, new_state));
                        }
                    }
                }

                if let Symbol::NonTerminal(non_terminal) = state.item.next_symbol() {
                    if let Some(prev_node) = h.get(non_terminal) {
                        let item = state.item.advanced();
                        let node = next_node(&state.node.borrow(), &item, i, prev_node, &mut v);

                        if item.is_finished() {
                            states[i].add(State::new(item, node));
                        } else {
                            match item.next_symbol() {
                                Symbol::NonTerminal(..) => {
                                    states[i].add(State::new(item, node));
                                }
                                Symbol::Terminal { token } => {
                                    expected_tokens
                                        .push(ExpectedToken::new(token.id, State::new(item, node)));
                                }
                            }
                        }
                    }
                }
            } else {
                if state.node.borrow().is_none() {
                    let node = Node::new(state.item.label(), i, i);
                    v.entry(node.clone()).or_default();
                    *state.node.borrow_mut() = Some(node);
                }

                if state.item.pos_input == i {
                    h.insert(state.item.rule.left, state.node.borrow().clone().unwrap());
                }

                for k in 0..states[state.item.pos_input].len() {
                    let s = &states[state.item.pos_input][k];
                    if !s.is_finished()
                        && s.item.next_symbol() == &Symbol::NonTerminal(state.item.rule.left)
                    {
                        let item = s.item.advanced();
                        let node = next_node(
                            &s.node.borrow(),
                            &item,
                            i,
                            state.node.borrow().as_ref().unwrap(),
                            &mut v,
                        );

                        if item.is_finished() {
                            states[i].add(State::new(item, node));
                        } else {
                            match item.next_symbol() {
                                Symbol::NonTerminal(..) => {
                                    states[i].add(State::new(item, node));
                                }
                                Symbol::Terminal { token } => {
                                    expected_tokens
                                        .push(ExpectedToken::new(token.id, State::new(item, node)));
                                }
                            }
                        }
                    }
                }
            }

            j += 1;
        }

        let token = lexer.next_token(strings)?;
        if token == Token::eof() {
            break;
        }
        if !expected_tokens.iter().any(|e| e.token_id == token.id) {
            return Err(CompilerError::unexpected_token(
                token,
                expected_tokens.into_iter().map(|e| e.token_id),
                strings,
            ));
        }

        let q = expected_tokens
            .drain(..)
            .filter_map(|e| {
                if e.token_id == token.id {
                    Some(e.next_state)
                } else {
                    None
                }
            })
            .collect::<BTreeSet<_>>();

        let prev_node = Node::new(NodeLabel::Symbol(Symbol::Terminal { token }), i, i + 1);
        states.push(RefSet::new());

        for state in q.into_iter() {
            let item = state.item.advanced();
            let node = next_node(&state.node.borrow(), &item, i + 1, &prev_node, &mut v);

            if item.is_finished() {
                states[i + 1].add(State::new(item, node));
            } else {
                match item.next_symbol() {
                    Symbol::NonTerminal(..) => {
                        states[i + 1].add(State::new(item, node));
                    }
                    Symbol::Terminal { token } => {
                        expected_tokens.push(ExpectedToken::new(token.id, State::new(item, node)));
                    }
                }
            }
        }

        h.clear();
        i += 1;
    }

    let states = states.last().unwrap();
    for i in 0..states.len() {
        let state = &states[i];

        if state.item.rule == &grammar.starting_rule
            && state.item.is_finished()
            && state.item.pos_input == 0
        {
            return Ok(ParseForest {
                families: v,
                root: state.node.borrow().clone().unwrap(),
            });
        }
    }

    Err(CompilerError::unexpected_eof())
}
