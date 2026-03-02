use std::path::PathBuf;

use compact_str::CompactString;
use rustc_hash::FxHashMap;

use crate::types::{
    ExportsKind, ImportKind, IndirectExportEntry, LocalExport, ModuleIdx, NamedImport,
    ResolvedImportRecord, StarExportEntry, SymbolRef, WrapKind,
};

/// Side-effects status for a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SideEffects {
    /// Has side effects (default).
    True,
    /// Declared side-effect-free (package.json or plugin).
    False,
    /// Cannot tree-shake (noTreeshake annotation).
    NoTreeshake,
}

impl SideEffects {
    /// Convert to the `Option<bool>` representation used by algorithms.
    ///
    /// - `True` → `Some(true)`
    /// - `False` → `Some(false)`
    /// - `NoTreeshake` → `None`
    pub fn to_option(self) -> Option<bool> {
        match self {
            Self::True => Some(true),
            Self::False => Some(false),
            Self::NoTreeshake => None,
        }
    }

    /// Create from the `Option<bool>` representation.
    pub fn from_option(opt: Option<bool>) -> Self {
        match opt {
            Some(true) => Self::True,
            Some(false) => Self::False,
            None => Self::NoTreeshake,
        }
    }
}

/// A normal (parseable) module in the module graph.
#[derive(Debug, Clone)]
pub struct NormalModule {
    // --- Identity ---
    /// This module's index in the graph.
    pub idx: ModuleIdx,
    /// Absolute file path.
    pub path: PathBuf,

    // --- Parse-time data ---
    /// Whether this module has ESM syntax (`import`/`export`).
    pub has_module_syntax: bool,
    /// The module's export format (ESM, CommonJS, or not yet determined).
    pub exports_kind: ExportsKind,
    /// Whether this module contains top-level `await`.
    pub has_top_level_await: bool,
    /// The module's own side-effects state (before propagation).
    pub side_effects: SideEffects,
    /// Whether this module has lazily-generated exports (Rolldown-specific).
    /// When true, static imports of this module do NOT force `exports_kind` to `Esm`.
    pub has_lazy_export: bool,
    /// Whether this module has side effects or global variable access
    /// that makes execution order significant.
    /// Used by on-demand wrapping: sensitive modules must always be wrapped.
    pub execution_order_sensitive: bool,

    // --- Import/export declarations ---
    // NOTE: `named_exports` and `named_imports` are consumed by `ModuleGraph::add_normal_module()`
    // and moved to graph-level storage. After adding, use `graph.named_exports_of(idx)` /
    // `graph.named_imports_of(idx)` to access them. The per-module fields will be empty.
    /// Named exports: export_name -> LocalExport.
    /// Moved to graph-level storage by `add_normal_module`.
    pub named_exports: FxHashMap<CompactString, LocalExport>,
    /// Named imports: local_symbol -> NamedImport.
    /// Moved to graph-level storage by `add_normal_module`.
    pub named_imports: FxHashMap<SymbolRef, NamedImport>,
    /// Import records (resolved).
    pub import_records: Vec<ResolvedImportRecord>,
    /// Star exports: `export * from '...'`
    pub star_export_entries: Vec<StarExportEntry>,
    /// Indirect exports: `export { x } from '...'`
    pub indirect_export_entries: Vec<IndirectExportEntry>,

    // --- Module-level symbols ---
    /// SymbolRef for default export.
    pub default_export_ref: SymbolRef,
    /// SymbolRef for namespace object (`import * as ns`).
    pub namespace_object_ref: SymbolRef,

    // --- Link-time results ---
    // NOTE: Most link-time results are stored at GRAPH level, not per-module.
    // Only `wrap_kind` and `has_dynamic_exports` live here because they are
    // read by downstream algorithms (wrap_modules, resolve_import).
    // All other results use graph accessors: `wrap_kind_of()`, `wrapper_ref_of()`,
    // `original_wrap_kind_of()`, `is_required_by_other_module()`,
    // `resolved_exports_of()`, `module_exec_order()`, `is_tla_or_contains_tla()`,
    // `propagated_side_effects_of()`.

    /// Wrapping strategy (set by determine_module_exports_kind / wrap_modules).
    /// The initial value from `determine_module_exports_kind` is read by `wrap_modules`.
    pub wrap_kind: WrapKind,
    /// Whether this module has dynamic exports (CJS or transitive `export *` from CJS/external).
    /// Read by `resolve_import` in `match_imports` for the NormalAndNamespace fallback.
    pub has_dynamic_exports: bool,
}

impl NormalModule {
    /// Create a new `NormalModule` with required fields; all others default.
    ///
    /// Consumers set optional fields via direct mutation:
    /// ```ignore
    /// let mut m = NormalModule::new(idx, path, default_ref, ns_ref);
    /// m.has_module_syntax = true;
    /// m.exports_kind = ExportsKind::Esm;
    /// ```
    pub fn new(
        idx: ModuleIdx,
        path: PathBuf,
        default_export_ref: SymbolRef,
        namespace_object_ref: SymbolRef,
    ) -> Self {
        Self {
            idx,
            path,
            default_export_ref,
            namespace_object_ref,
            has_module_syntax: false,
            exports_kind: ExportsKind::default(),
            has_top_level_await: false,
            side_effects: SideEffects::True,
            has_lazy_export: false,
            execution_order_sensitive: false,
            named_exports: FxHashMap::default(),
            named_imports: FxHashMap::default(),
            import_records: Vec::new(),
            star_export_entries: Vec::new(),
            indirect_export_entries: Vec::new(),
            wrap_kind: WrapKind::default(),
            has_dynamic_exports: false,
        }
    }

    /// Whether this module uses CommonJS exports.
    pub fn is_commonjs(&self) -> bool {
        self.exports_kind.is_commonjs()
    }

    /// Get the resolved target module for an import record by index.
    pub fn import_record_resolved_module(&self, idx: usize) -> Option<ModuleIdx> {
        self.import_records.get(idx)?.resolved_module
    }

    /// Iterate over static import dependencies only.
    pub fn static_dependencies(&self) -> impl Iterator<Item = ModuleIdx> + '_ {
        self.import_records.iter().filter_map(|rec| {
            if rec.kind == ImportKind::Static { rec.resolved_module } else { None }
        })
    }

    /// Iterate over star export target module indices.
    pub fn star_export_modules(&self) -> impl Iterator<Item = ModuleIdx> + '_ {
        self.star_export_entries.iter().filter_map(|e| e.resolved_module)
    }

}

/// An external (unresolvable) module — first-class node in the graph.
#[derive(Debug, Clone)]
pub struct ExternalModule {
    /// This module's index in the graph.
    pub idx: ModuleIdx,
    /// The specifier string (e.g., "react", "lodash").
    pub specifier: CompactString,
    /// Side-effects status.
    pub side_effects: SideEffects,
    /// SymbolRef for this module's namespace object.
    pub namespace_ref: SymbolRef,
}

/// A module in the graph — either normal (parseable) or external.
///
/// `NormalModule` is boxed to keep the enum small (~80 bytes instead of ~400+).
/// External module slots are common and benefit from reduced memory waste.
#[derive(Debug, Clone)]
pub enum Module {
    Normal(Box<NormalModule>),
    External(ExternalModule),
}

impl Module {
    /// Get the module index.
    pub fn idx(&self) -> ModuleIdx {
        match self {
            Self::Normal(m) => m.idx,
            Self::External(m) => m.idx,
        }
    }

    /// Get the side effects status.
    pub fn side_effects(&self) -> SideEffects {
        match self {
            Self::Normal(m) => m.side_effects,
            Self::External(m) => m.side_effects,
        }
    }

    /// Get as a normal module reference, if it is one.
    pub fn as_normal(&self) -> Option<&NormalModule> {
        match self {
            Self::Normal(m) => Some(m),
            Self::External(_) => None,
        }
    }

    /// Get as a mutable normal module reference, if it is one.
    pub fn as_normal_mut(&mut self) -> Option<&mut NormalModule> {
        match self {
            Self::Normal(m) => Some(m),
            Self::External(_) => None,
        }
    }

    /// Get as an external module reference, if it is one.
    pub fn as_external(&self) -> Option<&ExternalModule> {
        match self {
            Self::External(m) => Some(m),
            Self::Normal(_) => None,
        }
    }

    /// Get as a mutable external module reference, if it is one.
    pub fn as_external_mut(&mut self) -> Option<&mut ExternalModule> {
        match self {
            Self::External(m) => Some(m),
            Self::Normal(_) => None,
        }
    }

    /// Get the namespace object ref (for both normal and external modules).
    pub fn namespace_object_ref(&self) -> SymbolRef {
        match self {
            Self::Normal(m) => m.namespace_object_ref,
            Self::External(m) => m.namespace_ref,
        }
    }
}
