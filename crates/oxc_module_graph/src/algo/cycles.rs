use rustc_hash::{FxHashMap, FxHashSet};

use crate::graph::ModuleGraph;
use crate::module::Module;
use crate::types::ModuleIdx;

/// Find all cycles in the module dependency graph using DFS.
///
/// Returns a list of cycles, where each cycle is a sequence of module indices
/// forming a loop. Each cycle is reported once.
pub fn find_cycles(graph: &ModuleGraph) -> Vec<Vec<ModuleIdx>> {
    let mut cycles = Vec::new();
    let mut visited = FxHashSet::default();
    let mut stack = Vec::new();
    let mut path_map: FxHashMap<ModuleIdx, usize> = FxHashMap::default();

    for (idx, _) in graph.modules.iter_enumerated() {
        if !visited.contains(&idx) {
            dfs(graph, idx, &mut visited, &mut stack, &mut path_map, &mut cycles);
        }
    }

    cycles
}

fn dfs(
    graph: &ModuleGraph,
    node: ModuleIdx,
    visited: &mut FxHashSet<ModuleIdx>,
    stack: &mut Vec<ModuleIdx>,
    path_map: &mut FxHashMap<ModuleIdx, usize>,
    cycles: &mut Vec<Vec<ModuleIdx>>,
) {
    visited.insert(node);
    let pos = stack.len();
    path_map.insert(node, pos);
    stack.push(node);

    // Collect dependencies.
    let deps: Vec<ModuleIdx> = match &graph.modules[node] {
        Module::Normal(module) => module.edge_targets().map(|(_, t)| t).collect(),
        Module::External(_) => Vec::new(),
    };

    for target in deps {
        if !visited.contains(&target) {
            dfs(graph, target, visited, stack, path_map, cycles);
        } else if let Some(&cycle_start) = path_map.get(&target) {
            let cycle: Vec<ModuleIdx> = stack[cycle_start..].to_vec();
            cycles.push(cycle);
        }
    }

    stack.pop();
    path_map.remove(&node);
}
