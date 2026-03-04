use compact_str::CompactString;
use oxc_index::IndexVec;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::algo::BindingError;
use crate::default::SymbolRefDb;
use crate::module::{ExternalModule, Module, NormalModule};
use crate::types::{LocalExport, ModuleIdx, NamedImport, SymbolRef, WrapKind};
use oxc_syntax::symbol::SymbolId;

/// A concrete, batteries-included module graph.
///
/// Owns both the module storage and the symbol database.
/// Algorithms operate directly on `&ModuleGraph` / `&mut ModuleGraph`
/// instead of through trait bounds.
#[derive(Debug, Default, Clone)]
pub struct ModuleGraph {
    /// All modules (normal and external), indexed by `ModuleIdx`.
    pub modules: IndexVec<ModuleIdx, Module>,
    /// Cross-module symbol database with union-find linking.
    pub symbols: SymbolRefDb,

    /// When `true`, skip allocating symbol storage in `alloc_module_idx`,
    /// `add_normal_module`, and `add_external_module`. Consumers like Rolldown
    /// set this when they manage symbols via their own `SymbolRefDb`.
    skip_symbols: bool,

    /// Entry point module indices.
    entries: Vec<ModuleIdx>,
    /// Optional runtime module index.
    runtime: Option<ModuleIdx>,

    // --- Global link-time results ---
    /// Modules in execution order.
    exec_order: Vec<ModuleIdx>,
    /// Reverse index: module index → position in `exec_order`.
    /// `u32::MAX` means the module is not in the execution order.
    /// Built by `set_exec_order` for O(1) lookup.
    exec_order_index: IndexVec<ModuleIdx, u32>,
    /// Detected circular dependencies.
    cycles: Vec<Vec<ModuleIdx>>,
    /// Binding errors from import matching.
    binding_errors: Vec<BindingError>,

    // --- Graph-level wrapping results (populated by link() or WrapModulesResult::apply()) ---
    /// Finalized wrap_kind per module (from wrapping propagation).
    wrap_kinds: FxHashMap<ModuleIdx, WrapKind>,
    /// Wrapper symbol refs per module.
    wrapper_refs: FxHashMap<ModuleIdx, SymbolRef>,
    /// Original wrap_kind before propagation.
    original_wrap_kinds: FxHashMap<ModuleIdx, WrapKind>,
    /// Modules that are imported via `require()` by another module.
    required_by_other_module: FxHashSet<ModuleIdx>,

    // --- Graph-level resolved exports (populated by link() or set_resolved_exports()) ---
    /// Resolved exports per module. Stored at graph level to avoid cloning
    /// into every module struct. Use `resolved_exports_of()` to access.
    resolved_exports: FxHashMap<ModuleIdx, crate::algo::ResolvedExportsMap>,

    // --- Graph-level import/export declarations ---
    // Stored at graph level so consumers can populate them without copying
    // into NormalModule structs. Algorithms access via `named_exports_of()` /
    // `named_imports_of()`.
    /// Named exports per module: export_name -> LocalExport.
    named_exports: FxHashMap<ModuleIdx, FxHashMap<CompactString, LocalExport>>,
    /// Named imports per module: local_symbol -> NamedImport.
    named_imports: FxHashMap<ModuleIdx, FxHashMap<SymbolRef, NamedImport>>,

    // --- Graph-level import specifiers ---
    // Separated from `ResolvedImportRecord` to keep the per-record struct
    // compact (~24 bytes instead of ~56). Algorithms never need specifiers;
    // only the builder's `compute_is_type_only` and downstream consumers do.
    /// Import specifier strings per module, indexed in parallel with
    /// `NormalModule::import_records`.
    import_specifiers: FxHashMap<ModuleIdx, Vec<CompactString>>,

    // --- Graph-level algorithm output fields ---
    /// Modules affected by top-level await (directly or transitively).
    tla_modules: rustc_hash::FxHashSet<ModuleIdx>,
    /// Side-effects propagation results: (module_idx, has_side_effects).
    side_effects_results: FxHashMap<ModuleIdx, bool>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a `ModuleGraph` that skips SymbolRefDb allocation.
    ///
    /// Use this when the consumer manages its own symbol database (e.g.,
    /// Rolldown). The graph's `symbols` field will remain empty; calls to
    /// `alloc_module_idx`, `add_normal_module`, and `add_external_module`
    /// will not grow the symbol storage.
    pub fn without_symbols() -> Self {
        Self { skip_symbols: true, ..Self::default() }
    }

    // --- Build API ---

    /// Allocate a module index for a module that will be added later.
    pub fn alloc_module_idx(&mut self) -> ModuleIdx {
        let idx = self.modules.next_idx();
        // Push a placeholder — will be replaced by add_normal_module or add_external_module.
        // We use an external module placeholder since it's smaller.
        self.modules.push(Module::External(ExternalModule {
            idx,
            specifier: compact_str::CompactString::default(),
            side_effects: crate::module::SideEffects::True,
            namespace_ref: SymbolRef::new(idx, oxc_syntax::symbol::SymbolId::from_raw_unchecked(0)),
        }));
        if !self.skip_symbols {
            self.symbols.ensure_modules(idx.index() + 1);
        }
        idx
    }

    /// Add a normal module to the graph.
    ///
    /// The module's `idx` must match an allocated index.
    /// `named_exports` and `named_imports` are moved from the module to graph-level
    /// storage. After this call, use `named_exports_of(idx)` / `named_imports_of(idx)`
    /// to access them.
    pub fn add_normal_module(&mut self, mut module: NormalModule) {
        let idx = module.idx;
        // Build compact edge list from import_records (if not already built).
        if module.edges.is_empty() && !module.import_records.is_empty() {
            #[expect(clippy::cast_possible_truncation)]
            {
                module.edges = module
                    .import_records
                    .iter()
                    .map(|rec| crate::types::CompactImportEdge {
                        target: rec
                            .resolved_module
                            .map_or(crate::types::CompactImportEdge::UNRESOLVED, |m| {
                                m.index() as u32
                            }),
                        kind: rec.kind,
                    })
                    .collect();
            }
        }
        // Move named_exports/imports to graph-level storage.
        let exports = std::mem::take(&mut module.named_exports);
        let imports = std::mem::take(&mut module.named_imports);
        if !exports.is_empty() {
            self.named_exports.insert(idx, exports);
        }
        if !imports.is_empty() {
            self.named_imports.insert(idx, imports);
        }
        if idx.index() < self.modules.len() {
            self.modules[idx] = Module::Normal(Box::new(module));
        } else {
            self.extend_modules_to_fit(idx);
            self.modules.push(Module::Normal(Box::new(module)));
        }
        if !self.skip_symbols {
            self.symbols.ensure_modules(idx.index() + 1);
        }
    }

    /// Add an external module to the graph.
    ///
    /// The module's `idx` must match an allocated index.
    pub fn add_external_module(&mut self, module: ExternalModule) {
        let idx = module.idx;
        if idx.index() < self.modules.len() {
            self.modules[idx] = Module::External(module);
        } else {
            self.extend_modules_to_fit(idx);
            self.modules.push(Module::External(module));
        }
        if !self.skip_symbols {
            self.symbols.ensure_modules(idx.index() + 1);
        }
    }

    /// Extend `self.modules` with placeholder external modules until
    /// `self.modules.next_idx() == idx`.
    fn extend_modules_to_fit(&mut self, idx: ModuleIdx) {
        while self.modules.len() < idx.index() {
            let placeholder_idx = self.modules.next_idx();
            self.modules.push(Module::External(ExternalModule {
                idx: placeholder_idx,
                specifier: compact_str::CompactString::default(),
                side_effects: crate::module::SideEffects::True,
                namespace_ref: SymbolRef::new(
                    placeholder_idx,
                    oxc_syntax::symbol::SymbolId::from_raw_unchecked(0),
                ),
            }));
        }
        debug_assert_eq!(idx, self.modules.next_idx());
    }

    /// Add a symbol to the symbol database.
    pub fn add_symbol(&mut self, module: ModuleIdx, name: impl Into<CompactString>) -> SymbolRef {
        self.symbols.add_symbol(module, name)
    }

    /// Ensure the given module has symbol slots for at least `len` symbols.
    pub fn ensure_module_symbol_capacity(&mut self, module: ModuleIdx, len: usize) {
        self.symbols.ensure_module_symbol_capacity(module, len);
    }

    /// Set the name for an existing symbol slot.
    pub fn set_symbol_name(
        &mut self,
        module: ModuleIdx,
        symbol: SymbolId,
        name: impl Into<CompactString>,
    ) {
        self.symbols.set_symbol_name(module, symbol, name);
    }

    /// Initialize or reset a symbol slot to point to itself.
    pub fn init_symbol_self_link(&mut self, module: ModuleIdx, symbol: SymbolId) {
        self.symbols.init_symbol_self_link(module, symbol);
    }

    /// Allocate a new synthetic symbol in a module.
    pub fn alloc_synthetic_symbol(
        &mut self,
        module: ModuleIdx,
        name: impl Into<CompactString>,
    ) -> SymbolRef {
        self.symbols.alloc_synthetic_symbol(module, name)
    }

    /// Initialize all symbols for a module in one call.
    pub fn init_module_symbols(&mut self, module: ModuleIdx, names: &[CompactString]) {
        self.symbols.init_module_symbols(module, names);
    }

    /// Pre-allocate capacity for `additional` more modules.
    ///
    /// Useful when the total module count is known upfront (e.g. during a scan stage)
    /// to avoid repeated resizing.
    pub fn reserve_modules(&mut self, additional: usize) {
        self.modules.reserve(additional);
    }

    /// Set the entry points for the graph.
    pub fn set_entries(&mut self, entries: Vec<ModuleIdx>) {
        self.entries = entries;
    }

    /// Set the runtime module.
    pub fn set_runtime(&mut self, runtime: ModuleIdx) {
        self.runtime = Some(runtime);
    }

    // --- Query API ---

    /// Get a module by index.
    pub fn module(&self, idx: ModuleIdx) -> &Module {
        &self.modules[idx]
    }

    /// Get a module by index (mutable).
    pub fn module_mut(&mut self, idx: ModuleIdx) -> &mut Module {
        &mut self.modules[idx]
    }

    /// Get a normal module by index (returns `None` for external modules).
    pub fn normal_module(&self, idx: ModuleIdx) -> Option<&NormalModule> {
        self.modules.get(idx).and_then(Module::as_normal)
    }

    /// Get a mutable normal module by index.
    pub fn normal_module_mut(&mut self, idx: ModuleIdx) -> Option<&mut NormalModule> {
        self.modules.get_mut(idx).and_then(Module::as_normal_mut)
    }

    /// Get an external module by index.
    pub fn external_module(&self, idx: ModuleIdx) -> Option<&ExternalModule> {
        self.modules.get(idx).and_then(Module::as_external)
    }

    /// Number of modules (normal + external) in the graph.
    pub fn modules_len(&self) -> usize {
        self.modules.len()
    }

    /// Iterate over all normal modules.
    pub fn normal_modules(&self) -> impl Iterator<Item = &NormalModule> {
        self.modules.iter().filter_map(Module::as_normal)
    }

    /// Check if a symbol slot exists in the symbol database.
    pub fn has_symbol(&self, sym: SymbolRef) -> bool {
        self.symbols.has_symbol(sym)
    }

    /// Follow link chains to find the canonical (final) symbol.
    pub fn canonical_ref(&self, sym: SymbolRef) -> SymbolRef {
        self.symbols.canonical_ref_for(sym)
    }

    /// Follow link chains to find the canonical symbol, applying path halving.
    pub fn canonical_ref_mut(&mut self, sym: SymbolRef) -> SymbolRef {
        self.symbols.canonical_ref_for_mut(sym)
    }

    /// Link `from` to resolve to `to`.
    pub fn link_symbols(&mut self, from: SymbolRef, to: SymbolRef) {
        self.symbols.link(from, to);
    }

    /// Get the declared name of a symbol.
    pub fn symbol_name(&self, sym: SymbolRef) -> &str {
        self.symbols.symbol_name(sym)
    }

    /// Get the entry points.
    pub fn entries(&self) -> &[ModuleIdx] {
        &self.entries
    }

    /// Get the runtime module index.
    pub fn runtime(&self) -> Option<ModuleIdx> {
        self.runtime
    }

    // --- Link API ---

    /// Run the full link pipeline.
    ///
    /// This calls all algorithms in order and stores results in-place on each module.
    /// Consumers that need different ordering (like Rolldown, which runs
    /// `determine_module_exports_kind` between steps) can call individual
    /// algorithm methods instead.
    pub fn link(&mut self, config: &mut crate::hooks::LinkConfig) {
        use crate::algo;

        // 1. Execution order
        // Only store graph-level sorted order and cycles.
        // Per-module exec_order fields are NOT written — use
        // `module_exec_order(idx)` to look up a module's position instead.
        let exec = algo::compute_exec_order(self, config);
        self.exec_order = exec.sorted;
        self.cycles = exec.cycles;

        // 2. TLA propagation — stored at graph level.
        // Use `is_tla_or_contains_tla(idx)` to query after link().
        self.tla_modules = algo::compute_tla(self);

        // 2b. Determine module exports kind
        // NOTE: These per-module writes ARE intentional — `exports_kind` and
        // initial `wrap_kind` are read by downstream algorithms:
        //   - `wrap_modules` reads `module.exports_kind` and `module.wrap_kind`
        //   - `compute_has_dynamic_exports` reads `module.is_commonjs()`
        //   - `build_resolved_exports` reads `module.is_commonjs()`
        let ek_config =
            algo::ExportsKindConfig { dynamic_imports_as_require: false, wrap_cjs_entries: true };
        let ek_result = algo::determine_module_exports_kind(self, &ek_config);
        for (idx, kind) in &ek_result.exports_kind_updates {
            if let Module::Normal(m) = &mut self.modules[*idx] {
                m.exports_kind = *kind;
            }
        }
        for (idx, wrap) in &ek_result.wrap_kind_updates {
            if let Module::Normal(m) = &mut self.modules[*idx] {
                m.wrap_kind = *wrap;
            }
        }

        // 2c. Wrap modules
        // Only store results at graph level — no downstream algorithm reads
        // per-module wrap_kind/wrapper_ref/original_wrap_kind/required_by_other_module.
        // Use `wrap_kind_of()`, `wrapper_ref_of()`, etc. to access after link().
        let wrap_config = algo::WrapModulesConfig {
            on_demand_wrapping: false,
            strict_execution_order: false,
            skip_symbol_creation: false,
        };
        let wrap_result = algo::wrap_modules(self, &wrap_config);
        self.wrap_kinds = wrap_result.wrap_kind_updates;
        self.wrapper_refs = wrap_result.wrapper_refs;
        self.original_wrap_kinds = wrap_result.original_wrap_kinds;
        self.required_by_other_module = wrap_result.required_by_other_module;

        // 3. Dynamic exports (runs after exports_kind is finalized)
        // NOTE: This per-module write IS intentional (unlike exec_order, wrapping,
        // and resolved_exports which are stored at graph level only).
        // `has_dynamic_exports` is read by `resolve_import` in step 5 via
        // `target.has_dynamic_exports` for the NormalAndNamespace fallback path.
        let dynamic = algo::compute_has_dynamic_exports(self);
        for &idx in &dynamic {
            if let Module::Normal(m) = &mut self.modules[idx] {
                m.has_dynamic_exports = true;
            }
        }

        // 4. Resolved exports — stored at graph level only.
        // No per-module `resolved_exports` writes; the map is passed directly
        // to match_imports_collect_with and stored at graph level afterward.
        let resolved = algo::build_resolved_exports(self);

        // 5. Match imports (with built-in CJS interop)
        let (errors, links) = algo::match_imports_collect_with(self, config, Some(&resolved));
        for (from, to) in links {
            self.symbols.link(from, to);
        }
        self.binding_errors = errors;
        self.resolved_exports = resolved;

        // 6. Side effects propagation — stored at graph level.
        // Use `propagated_side_effects_of(idx)` to query after link().
        self.side_effects_results = algo::determine_side_effects(self, config).into_iter().collect();
    }

    /// Flatten all symbol union-find chains so every symbol points directly
    /// to its root. After calling this, all `canonical_ref()` calls are O(1).
    pub fn flatten_symbol_chains(&mut self) {
        self.symbols.flatten_all_chains();
    }

    // --- Post-link queries ---

    /// Get module indices in execution order.
    pub fn exec_order(&self) -> &[ModuleIdx] {
        &self.exec_order
    }

    /// Look up a module's execution order position (O(1)).
    ///
    /// Returns `None` if the module was not visited during execution order
    /// computation (e.g., unreachable modules or if `link()` hasn't been called).
    pub fn module_exec_order(&self, idx: ModuleIdx) -> Option<u32> {
        self.exec_order_index
            .get(idx)
            .copied()
            .filter(|&pos| pos != u32::MAX)
    }

    /// Get detected circular dependencies.
    pub fn cycles(&self) -> &[Vec<ModuleIdx>] {
        &self.cycles
    }

    /// Get binding errors from import matching.
    pub fn binding_errors(&self) -> &[BindingError] {
        &self.binding_errors
    }

    /// Set binding errors (used by algorithms).
    pub(crate) fn set_binding_errors(&mut self, errors: Vec<BindingError>) {
        self.binding_errors = errors;
    }

    /// Set execution order (used by `ExecOrderResult::apply`).
    ///
    /// Also builds the reverse index for O(1) `module_exec_order()` lookup.
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn set_exec_order(&mut self, order: Vec<ModuleIdx>) {
        // Build reverse index: module_idx → position in exec_order.
        let module_count = self.modules.len();
        let mut index = IndexVec::from_vec(vec![u32::MAX; module_count]);
        for (pos, &module_idx) in order.iter().enumerate() {
            if module_idx.index() < module_count {
                index[module_idx] = pos as u32;
            }
        }
        self.exec_order_index = index;
        self.exec_order = order;
    }

    /// Set detected cycles (used by `ExecOrderResult::apply`).
    pub(crate) fn set_cycles(&mut self, cycles: Vec<Vec<ModuleIdx>>) {
        self.cycles = cycles;
    }

    /// Store wrapping results at graph level (used by `WrapModulesResult::apply`).
    pub(crate) fn set_wrap_results(
        &mut self,
        wrap_kinds: FxHashMap<ModuleIdx, WrapKind>,
        wrapper_refs: FxHashMap<ModuleIdx, SymbolRef>,
        original_wrap_kinds: FxHashMap<ModuleIdx, WrapKind>,
        required_by_other_module: FxHashSet<ModuleIdx>,
    ) {
        self.wrap_kinds = wrap_kinds;
        self.wrapper_refs = wrapper_refs;
        self.original_wrap_kinds = original_wrap_kinds;
        self.required_by_other_module = required_by_other_module;
    }

    // --- Post-link wrapping accessors ---

    /// Get the finalized wrap_kind for a module (from wrapping propagation).
    ///
    /// Returns the propagated wrap_kind if set, otherwise falls back to the
    /// per-module value (from `determine_module_exports_kind`).
    pub fn wrap_kind_of(&self, idx: ModuleIdx) -> WrapKind {
        self.wrap_kinds
            .get(&idx)
            .copied()
            .or_else(|| self.normal_module(idx).map(|m| m.wrap_kind))
            .unwrap_or(WrapKind::None)
    }

    /// Get the wrapper symbol ref for a module (if it was wrapped).
    pub fn wrapper_ref_of(&self, idx: ModuleIdx) -> Option<SymbolRef> {
        self.wrapper_refs.get(&idx).copied()
    }

    /// Get the original wrap_kind before propagation.
    pub fn original_wrap_kind_of(&self, idx: ModuleIdx) -> WrapKind {
        self.original_wrap_kinds.get(&idx).copied().unwrap_or(WrapKind::None)
    }

    /// Check whether a module is imported via `require()` by another module.
    pub fn is_required_by_other_module(&self, idx: ModuleIdx) -> bool {
        self.required_by_other_module.contains(&idx)
    }

    /// Get all graph-level wrap_kind entries.
    pub fn wrap_kinds(&self) -> &FxHashMap<ModuleIdx, WrapKind> {
        &self.wrap_kinds
    }

    /// Get all graph-level wrapper_ref entries.
    pub fn wrapper_refs(&self) -> &FxHashMap<ModuleIdx, SymbolRef> {
        &self.wrapper_refs
    }

    /// Get all graph-level original_wrap_kind entries.
    pub fn original_wrap_kinds(&self) -> &FxHashMap<ModuleIdx, WrapKind> {
        &self.original_wrap_kinds
    }

    /// Get modules that are imported via `require()` by another module.
    pub fn required_by_other_modules(&self) -> &FxHashSet<ModuleIdx> {
        &self.required_by_other_module
    }

    // --- Post-link TLA and side-effects accessors ---

    /// Check whether a module is affected by top-level await (directly or transitively).
    pub fn is_tla_or_contains_tla(&self, idx: ModuleIdx) -> bool {
        self.tla_modules.contains(&idx)
    }

    /// Get all modules affected by top-level await.
    pub fn tla_modules(&self) -> &rustc_hash::FxHashSet<ModuleIdx> {
        &self.tla_modules
    }

    /// Get the propagated side-effects status for a module.
    ///
    /// Returns `None` if side-effects propagation hasn't been run.
    pub fn propagated_side_effects_of(&self, idx: ModuleIdx) -> Option<bool> {
        self.side_effects_results.get(&idx).copied()
    }

    // --- Post-link resolved exports accessors ---

    /// Store resolved exports at graph level.
    pub(crate) fn set_resolved_exports(
        &mut self,
        resolved: FxHashMap<ModuleIdx, crate::algo::ResolvedExportsMap>,
    ) {
        self.resolved_exports = resolved;
    }

    /// Get the graph-level resolved exports map for a module.
    ///
    /// Returns `None` if the module has no resolved exports or if
    /// `link()` hasn't been called.
    pub fn resolved_exports_of(
        &self,
        idx: ModuleIdx,
    ) -> Option<&crate::algo::ResolvedExportsMap> {
        self.resolved_exports.get(&idx)
    }

    /// Get the full graph-level resolved exports map.
    pub fn resolved_exports(&self) -> &FxHashMap<ModuleIdx, crate::algo::ResolvedExportsMap>
    {
        &self.resolved_exports
    }

    // --- Named exports / imports accessors ---

    /// Get the named exports for a module.
    pub fn named_exports_of(
        &self,
        idx: ModuleIdx,
    ) -> Option<&FxHashMap<CompactString, LocalExport>> {
        self.named_exports.get(&idx)
    }

    /// Get the named imports for a module.
    pub fn named_imports_of(
        &self,
        idx: ModuleIdx,
    ) -> Option<&FxHashMap<SymbolRef, NamedImport>> {
        self.named_imports.get(&idx)
    }

    /// Set named exports for a module (graph-level storage).
    pub fn set_named_exports(
        &mut self,
        idx: ModuleIdx,
        exports: FxHashMap<CompactString, LocalExport>,
    ) {
        self.named_exports.insert(idx, exports);
    }

    /// Set named imports for a module (graph-level storage).
    pub fn set_named_imports(
        &mut self,
        idx: ModuleIdx,
        imports: FxHashMap<SymbolRef, NamedImport>,
    ) {
        self.named_imports.insert(idx, imports);
    }

    // --- Import specifier accessors ---

    /// Get the specifier string for a module's import record at `record_idx`.
    ///
    /// Returns `None` if no specifiers are stored for this module or if
    /// `record_idx` is out of bounds.
    pub fn import_specifier_of(&self, module: ModuleIdx, record_idx: usize) -> Option<&str> {
        self.import_specifiers
            .get(&module)
            .and_then(|specs| specs.get(record_idx))
            .map(CompactString::as_str)
    }

    /// Store import specifier strings for a module (graph-level storage).
    ///
    /// The Vec is indexed in parallel with the module's `import_records`.
    pub fn set_import_specifiers(&mut self, module: ModuleIdx, specifiers: Vec<CompactString>) {
        if !specifiers.is_empty() {
            self.import_specifiers.insert(module, specifiers);
        }
    }

    /// Get all import specifiers for a module.
    pub fn import_specifiers_of(&self, module: ModuleIdx) -> Option<&[CompactString]> {
        self.import_specifiers.get(&module).map(Vec::as_slice)
    }

    /// Check whether a module provides a given export name.
    ///
    /// Checks four sources of exports:
    /// 1. `has_dynamic_exports` (CJS / transitive `export *` from CJS) — suppresses warnings
    /// 2. `named_exports` (graph-level: local `export { x }` / `export const x`)
    /// 3. `resolved_exports` (graph-level: populated after `build_resolved_exports` / link)
    /// 4. `indirect_export_entries` (`export { x } from './foo'`)
    ///
    /// Returns `true` if any of these sources can provide `name`.
    pub fn has_export(&self, idx: ModuleIdx, name: &str) -> bool {
        if let Some(module) = self.normal_module(idx) {
            module.has_dynamic_exports
                || self.named_exports.get(&idx).is_some_and(|e| e.contains_key(name))
                || self.resolved_exports.get(&idx).is_some_and(|e| e.contains_key(name))
                || module
                    .indirect_export_entries
                    .iter()
                    .any(|e| e.exported_name.as_str() == name)
        } else {
            false
        }
    }

    /// If the given symbol is a named import in a module, return its import info.
    ///
    /// Returns `Some((imported_name, record_idx, is_namespace))`.
    pub fn symbol_import_info(
        &self,
        module_idx: ModuleIdx,
        symbol: SymbolRef,
    ) -> Option<(&str, usize, bool)> {
        let import = self.named_imports.get(&module_idx)?.get(&symbol)?;
        let is_ns = import.imported_name.as_str() == "*";
        Some((import.imported_name.as_str(), import.record_idx.index(), is_ns))
    }
}
