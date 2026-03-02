use oxc_index::IndexVec;
use oxc_syntax::symbol::SymbolId;

use crate::types::{ModuleIdx, SymbolRef};

/// Per-module payload storage for consumer-specific data.
///
/// Stores one `T` value per `ModuleIdx`, aligned with `ModuleGraph::modules`.
/// Use this to attach heavy generate-only data (e.g., source text, mutations,
/// sourcemap chains, statement info) without bloating `NormalModule`.
///
/// # Example
///
/// ```rust,ignore
/// use oxc_module_graph::{ModuleGraph, ModulePayloads};
///
/// struct MyModuleData {
///     source: String,
///     sourcemap_chain: Vec<String>,
/// }
///
/// let graph = ModuleGraph::new();
/// let mut payloads: ModulePayloads<MyModuleData> = ModulePayloads::new();
///
/// // Store data for a module
/// payloads.set(idx, MyModuleData { source: "..".into(), sourcemap_chain: vec![] });
///
/// // Access later
/// if let Some(data) = payloads.get(idx) {
///     println!("{}", data.source);
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ModulePayloads<T> {
    inner: IndexVec<ModuleIdx, Option<T>>,
}

impl<T> Default for ModulePayloads<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> ModulePayloads<T> {
    pub fn new() -> Self {
        Self { inner: IndexVec::new() }
    }

    /// Create with pre-allocated capacity for `n` modules.
    pub fn with_capacity(n: usize) -> Self {
        Self { inner: IndexVec::with_capacity(n) }
    }

    /// Get the payload for a module.
    pub fn get(&self, idx: ModuleIdx) -> Option<&T> {
        self.inner.get(idx).and_then(Option::as_ref)
    }

    /// Get a mutable reference to the payload for a module.
    pub fn get_mut(&mut self, idx: ModuleIdx) -> Option<&mut T> {
        self.inner.get_mut(idx).and_then(Option::as_mut)
    }

    /// Set the payload for a module, returning the previous value if any.
    pub fn set(&mut self, idx: ModuleIdx, value: T) -> Option<T> {
        // Extend with None if needed.
        while self.inner.len() <= idx.index() {
            self.inner.push(None);
        }
        self.inner[idx].replace(value)
    }

    /// Remove the payload for a module, returning it if present.
    pub fn take(&mut self, idx: ModuleIdx) -> Option<T> {
        self.inner.get_mut(idx).and_then(Option::take)
    }

    /// Iterate over all (index, payload) pairs where the payload is present.
    pub fn iter(&self) -> impl Iterator<Item = (ModuleIdx, &T)> {
        self.inner.iter_enumerated().filter_map(|(idx, opt)| opt.as_ref().map(|v| (idx, v)))
    }

    /// Iterate mutably over all (index, payload) pairs where the payload is present.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (ModuleIdx, &mut T)> {
        self.inner.iter_mut().enumerate().filter_map(|(i, opt)| {
            opt.as_mut().map(|v| (ModuleIdx::from_usize(i), v))
        })
    }

    /// Number of modules with payloads set.
    pub fn count(&self) -> usize {
        self.inner.iter().filter(|opt| opt.is_some()).count()
    }
}

/// Per-symbol payload storage for consumer-specific data.
///
/// Stores one `T` value per `SymbolRef` (module + symbol pair), using a
/// two-level structure: outer `IndexVec<ModuleIdx, Vec<Option<T>>>`,
/// inner `Vec` indexed by `SymbolId`.
///
/// Use this to attach per-symbol extras (e.g., namespace alias, chunk index,
/// flags, scope info) without bloating `SymbolRefDb`.
///
/// # Example
///
/// ```rust,ignore
/// use oxc_module_graph::{SymbolPayloads, SymbolRef};
///
/// struct MySymbolData {
///     chunk_idx: u32,
///     flags: u32,
/// }
///
/// let mut payloads: SymbolPayloads<MySymbolData> = SymbolPayloads::new();
/// payloads.set(sym_ref, MySymbolData { chunk_idx: 0, flags: 0 });
/// ```
#[derive(Debug, Clone)]
pub struct SymbolPayloads<T> {
    inner: IndexVec<ModuleIdx, Vec<Option<T>>>,
}

impl<T> Default for SymbolPayloads<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SymbolPayloads<T> {
    pub fn new() -> Self {
        Self { inner: IndexVec::new() }
    }

    /// Get the payload for a symbol.
    pub fn get(&self, sym: SymbolRef) -> Option<&T> {
        self.inner
            .get(sym.owner)
            .and_then(|syms| syms.get(sym.symbol.index()))
            .and_then(Option::as_ref)
    }

    /// Get a mutable reference to the payload for a symbol.
    pub fn get_mut(&mut self, sym: SymbolRef) -> Option<&mut T> {
        self.inner
            .get_mut(sym.owner)
            .and_then(|syms| syms.get_mut(sym.symbol.index()))
            .and_then(Option::as_mut)
    }

    /// Set the payload for a symbol, returning the previous value if any.
    pub fn set(&mut self, sym: SymbolRef, value: T) -> Option<T> {
        // Extend outer vec if needed.
        while self.inner.len() <= sym.owner.index() {
            self.inner.push(Vec::new());
        }
        let syms = &mut self.inner[sym.owner];
        // Extend inner vec if needed.
        let id = sym.symbol.index();
        while syms.len() <= id {
            syms.push(None);
        }
        syms[id].replace(value)
    }

    /// Remove the payload for a symbol, returning it if present.
    pub fn take(&mut self, sym: SymbolRef) -> Option<T> {
        self.inner
            .get_mut(sym.owner)
            .and_then(|syms| syms.get_mut(sym.symbol.index()))
            .and_then(Option::take)
    }

    /// Ensure capacity for a module's symbols.
    pub fn ensure_module_capacity(&mut self, module: ModuleIdx, symbol_count: usize) {
        while self.inner.len() <= module.index() {
            self.inner.push(Vec::new());
        }
        let syms = &mut self.inner[module];
        if syms.len() < symbol_count {
            syms.resize_with(symbol_count, || None);
        }
    }

    /// Iterate over all (SymbolRef, payload) pairs where the payload is present.
    pub fn iter(&self) -> impl Iterator<Item = (SymbolRef, &T)> {
        self.inner.iter_enumerated().flat_map(|(mod_idx, syms)| {
            syms.iter().enumerate().filter_map(move |(sym_idx, opt)| {
                opt.as_ref().map(|v| {
                    #[expect(clippy::cast_possible_truncation)]
                    let sym =
                        SymbolRef::new(mod_idx, SymbolId::from_raw_unchecked(sym_idx as u32));
                    (sym, v)
                })
            })
        })
    }
}
