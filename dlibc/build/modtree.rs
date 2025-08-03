use clang_sys::support::Clang;
use compact_str::{CompactString, ToCompactString};
use petgraph::Graph;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::{fs, io, mem};
use syn::visit::Visit;
use syn::{Item, ItemEnum, ItemStruct, ItemType, ItemUnion, Type};

const DISABLE_WARNING_LINTS: &str = "#![allow(dead_code, \
		mutable_transmutes, \
		non_camel_case_types, \
		non_snake_case, \
		non_upper_case_globals, \
		unused, \
		unsafe_code,\
		unsafe_op_in_unsafe_fn,\
		ambiguous_glob_reexports, \
		ambiguous_glob_imports, \
		improper_ctypes, \
		unnecessary_transmutes \
		)]\n";

type HeaderPath = Rc<str>;
type ModulePath = Rc<str>;
type SymbolName = CompactString;

#[derive(Debug, Clone, Default)]
pub struct ModSym {
	pub defined: HashSet<SymbolName>,
	pub used: HashSet<SymbolName>,
}

impl Visit<'_> for ModSym {
	fn visit_item_enum(&mut self, i: &ItemEnum) {
		self.defined.insert(i.ident.to_compact_string());
		syn::visit::visit_item_enum(self, i);
	}
	fn visit_item_struct(&mut self, i: &ItemStruct) {
		self.defined.insert(i.ident.to_compact_string());
		syn::visit::visit_item_struct(self, i);
	}
	fn visit_item_type(&mut self, i: &ItemType) {
		self.defined.insert(i.ident.to_compact_string());
		syn::visit::visit_item_type(self, i);
	}
	fn visit_item_union(&mut self, i: &ItemUnion) {
		self.defined.insert(i.ident.to_compact_string());
		syn::visit::visit_item_union(self, i);
	}

	// 捕获类型使用
	fn visit_type(&mut self, node: &syn::Type) {
		match node {
			Type::Path(type_path) => {
				if let Some(ident) = type_path.path.get_ident() {
					self.used.get_or_insert(ident.to_compact_string());
				}
			}
			_ => {}
		}

		syn::visit::visit_type(self, node);
	}
}

#[derive(Debug, Clone, Default)]
pub struct ModuleTree {
	pub name: HeaderPath,
	/// 原始头文件导入
	pub includes: HashSet<HeaderPath>,
	pub tokens: Vec<TokenStream>,
	pub syms: ModSym,
}
#[derive(Debug, Clone, Default)]
pub struct Includes {
	pub mod_map: BTreeMap<HeaderPath, Rc<RefCell<ModuleTree>>>,
	pub mod_name_map: BTreeMap<HeaderPath, ModulePath>,
	pub graph: GraphData,
	pub tree_graph: Modtree,
}
#[derive(Debug, Clone, Default)]
pub struct GraphData {
	pub graph: Graph<Node, Edge>,
	pub module_nodes: HashMap<HeaderPath, NodeIndex>,
	pub symbol_nodes: HashMap<CompactString, Vec<NodeIndex>>,
}
impl GraphData {
	pub fn get_symbol_header(&self, symbol_index: NodeIndex) -> Option<HeaderPath> {
		let source = self.get_symbol_header_id(symbol_index)?;

		if let Node::Header(header) = self.graph[source].clone() {
			return Some(header);
		}
		None
	}
	pub fn get_symbol_header_id(&self, symbol_index: NodeIndex) -> Option<NodeIndex> {
		self.graph.edges_directed(symbol_index, petgraph::Incoming).find_map(|edge| {
			if *edge.weight() == Edge::Define {
				Some(edge.source())
			} else {
				None
			}
		})
	}
}

#[derive(Clone, Default, Debug, PartialEq)]
pub enum Node {
	#[default]
	None,
	Header(HeaderPath),
	Symbol(SymbolName),
}
impl Display for Node {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			Node::None => write!(f, "None"),
			Node::Header(header) => write!(f, "Header({})", header),
			Node::Symbol(symbol) => write!(f, "Symbol({})", symbol),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Edge {
	Include {
		order: u32,
	},
	Define,
	// facade -> header
	ExportGlob,
	// (Facade) -> (Symbol)
	ExportsExplicit {
		source_module: HeaderPath, // 来源于哪个 Rust 模块名
	},
}

impl Default for Edge {
	fn default() -> Self {
		Self::Define
	}
}

pub type TreeNode = CompactString;

#[derive(Debug, Clone, PartialEq)]
pub enum TreeEdge {
	SubMod,
	Subdir,
	Parent,
}
#[derive(Debug, Clone, Default)]
pub struct Modtree {
	pub graph: Graph<TreeNode, TreeEdge>,
	pub leaf_nodes: HashMap<Rc<str>, NodeIndex>,
	pub root: NodeIndex,
}
impl Modtree {
	pub fn from_headers<T: Iterator<Item = Rc<str>>>(headers: T) -> Self {
		let mut g = Graph::<TreeNode, TreeEdge>::new();
		let mut path_to_node = HashMap::new();
		let mut leaf_nodes = HashMap::new(); // 存储叶子节点（文件节点）

		// 创建根节点
		let root_index = g.add_node("".into());
		path_to_node.insert("/".to_string(), root_index);
		let mut current_path = String::new();
		for header_ in headers {
			let header = path_to_mod_path(header_.trim_end_matches(".h").as_str());
			let parts: Vec<&str> = header.split('/').filter(|s| !s.is_empty()).collect();
			let mut parent = root_index;
			current_path.clear();

			for (i, part) in parts.iter().enumerate() {
				current_path.push('/');
				current_path.push_str(part);

				let is_leaf = i == parts.len() - 1;
				if !is_leaf {
					current_path.push('/');
				}
				let node_index = *path_to_node.entry(current_path.clone()).or_insert_with(|| {
					let mut node_name: TreeNode = part.to_compact_string();

					let cur_edge = if is_leaf { TreeEdge::SubMod } else { TreeEdge::Subdir };
					// 对mod 名称与目录名称冲突的情况进行插入时解决
					if let Some(e) = g.edges_directed(parent, petgraph::Outgoing).find_map(|edge| {
						if *edge.weight() != TreeEdge::Parent && *edge.weight() != cur_edge && g[edge.target()] == part
						{
							Some(edge.target())
						} else {
							None
						}
					}) {
						if is_leaf {
							g[e].push('_');
						} else {
							node_name.push('_');
						}
					}

					let node = g.add_node(node_name);
					// 添加目录关系边
					g.add_edge(parent, node, cur_edge);
					// 添加父目录反向链接
					g.add_edge(node, parent, TreeEdge::Parent);
					node
				});

				parent = node_index;

				// 如果是叶子节点（文件），存储到leaf_nodes
				if is_leaf {
					leaf_nodes.insert(header_.clone(), node_index);
				}
			}
		}
		Self { graph: g, root: root_index, leaf_nodes }
	}
	pub fn get_leaf_node(&self, header: &str) -> Option<NodeIndex> {
		self.leaf_nodes.get(header).map(|i| *i)
	}
	/// 生成rust 安全的mod 路径, 针对如
	/// - `/usr/include/x86_64-linux-gnu/bits/types/idtype_t.h`
	/// - `/usr/include/x86_64-linux-gnu/bits/types.h`
	/// 此类mod 名称与目录名称冲突的问题
	pub fn get_safe_mod_name(&self, path: &str) -> String {
		let left_node = match self.get_leaf_node(path) {
			Some(node) => node,
			None => return "".into(),
		};
		let mut cur_node: NodeIndex = left_node;

		let mut seps = Vec::with_capacity(8);
		seps.push(self.graph[left_node].clone());

		let mut prev_is_mod = true;
		let mut prev_name = CompactString::new("");
		loop {
			if let Some(e) = self.graph.edges_directed(cur_node, petgraph::Outgoing).find_map(|edge| {
				if *edge.weight() == TreeEdge::Parent {
					Some(edge.target())
				} else {
					None
				}
			}) {
				if e == self.root {
					break;
				}
				let cur_dir = self.graph[e].clone();

				if self.graph.edges_directed(e, petgraph::Outgoing).any(|edge| {
					if (!prev_is_mod) && *edge.weight() == TreeEdge::SubMod && self.graph[edge.target()] == prev_name {
						true
					} else {
						false
					}
				}) {
					if let Some(_s) = seps.last_mut() {
						// s.push('_');
					}
				}
				prev_is_mod = false;
				prev_name = cur_dir.clone();
				cur_node = e;
				seps.push(cur_dir);
			} else {
				break;
			}
		}
		seps.push("".into());
		seps.reverse();
		seps.join("::")
	}
	pub fn get_safe_mod_path(&self, path: &str) -> String {
		let mod_name = self.get_safe_mod_name(path);
		mod_name.trim_start_matches("::").replace("::", "/")
	}
}

impl Includes {
	/// 对全局符号去重
	/// bindgen 在生成头文件时, 可能存在某条路径没有生成如[__BindgenUnionField] 的符号,
	/// 需要全收集起来,并对齐去重
	pub fn global_mod_dedup(&mut self) {
		if !self.mod_map.contains_key("") {
			return;
		}

		let global_mod = self.mod_map.get("").unwrap();
		if global_mod.borrow().tokens.is_empty() {
			return;
		}
		let tokens = mem::replace(global_mod.borrow_mut().tokens.as_mut(), Vec::with_capacity(1));
		let token: TokenStream = tokens.into_iter().collect();
		let new = dedup_bindings(&token);
		global_mod.borrow_mut().tokens.push(new);
	}

	pub fn parse_mod_sym(&mut self) {
		for module in self.mod_map.values_mut() {
			let mut m = module.borrow_mut();
			let mut mod_syms = ModSym::default();
			for token in &m.tokens {
				mod_syms.visit_file(&syn::parse_quote!(#token));
			}
			m.syms = mod_syms;
		}
	}

	pub fn parse_treegraph(&mut self) {
		let treemods = Modtree::from_headers(self.mod_name_map.keys().map(|x| x.clone()));
		self.tree_graph = treemods;
	}

	pub fn parse_graph(&mut self) {
		let mut graph: Graph<Node, Edge> = Graph::new();
		let mut header_node_map = HashMap::new();

		let mut symbol_nodes: HashMap<SymbolName, Vec<NodeIndex>> = HashMap::new();

		for header in self.mod_map.values() {
			// 定义
			let header_node = graph.add_node(Node::Header(header.borrow().name.clone()));
			header_node_map.insert(header.borrow().name.clone(), header_node);
			for define in header.borrow().syms.defined.iter() {
				let sym_node = graph.add_node(Node::Symbol(define.clone()));
				if let Some(sym) = symbol_nodes.get_mut(define) {
					sym.push(sym_node);
				} else {
					symbol_nodes.insert(define.clone(), vec![sym_node]);
				}
				graph.add_edge(header_node, sym_node, Edge::Define);
			}
		}
		// 头文件依赖
		for header in self.mod_map.values() {
			let header_node = header_node_map.get(header.borrow().name.as_str()).unwrap();
			for (idx, inc) in header.borrow().includes.iter().enumerate() {
				let hn_inc = header_node_map.get(inc).unwrap();
				graph.add_edge(*header_node, *hn_inc, Edge::Include { order: (idx + 1) as u32 });
			}
		}

		self.graph = GraphData { graph, ..Default::default() };
		// 解决不完整头文件依赖的符号, 如被嵌入到其他头文件的情况
		for (_, header_node) in header_node_map.iter() {
			self.resolve_undefined_symbols(*header_node, &symbol_nodes);
		}
		// 解决导出符号时冲突
		for (_, header_node) in header_node_map.iter() {
			Self::resolve_exports(&mut self.graph.graph, *header_node);
		}
		self.graph.module_nodes = header_node_map;
		self.graph.symbol_nodes = symbol_nodes;
	}

	pub fn resolve_undefined_symbols(
		&mut self,
		facade_node: NodeIndex,
		symbol_nodes: &HashMap<SymbolName, Vec<NodeIndex>>,
	) {
		let provided_symbols = Self::get_all_provided_symbols(&self.graph.graph, facade_node);
		let header_name = if let Node::Header(header) = self.graph.graph[facade_node].clone() {
			header
		} else {
			return;
		};

		let header = self.mod_map.get(&header_name).unwrap().clone();

		for used in header.borrow().syms.used.difference(&header.borrow().syms.defined) {
			if let Some(symbols) = symbol_nodes.get(used) {
				if symbols.iter().any(|x| provided_symbols.contains(x)) {
					continue;
				}
				if symbols.len() == 1 {
					if let Some(header_name) = self.graph.get_symbol_header(symbols[0]) {
						self.graph.graph.add_edge(
							facade_node,
							symbols[0],
							Edge::ExportsExplicit { source_module: header_name },
						);
					}
					continue;
				}

				let tree_facade_node = match self.tree_graph.get_leaf_node(header_name.as_str()) {
					Some(node) => node,
					None => continue,
				};

				let mut min_cost = u32::MAX;
				let mut min_cost_node = facade_node;

				for symbol in symbols.iter() {
					assert_eq!(self.graph.graph[*symbol], Node::Symbol(used.clone()));
					let symbol_header = match self.graph.get_symbol_header(*symbol) {
						Some(header) => header,
						None => {
							eprintln!("cannot get symbol header {:?}", self.graph.graph[*symbol]);
							continue;
						}
					};
					let tree_node = match self.tree_graph.get_leaf_node(symbol_header.as_str()) {
						Some(node) => node,
						None => continue,
					};

					// 找到最近的定义的header
					use petgraph::algo::astar::astar;
					if let Some((c, _)) = astar(
						&self.tree_graph.graph,
						tree_facade_node,
						|x| x == tree_node,
						|e| match e.weight() {
							TreeEdge::SubMod => 1u32,
							TreeEdge::Subdir => 1u32,
							TreeEdge::Parent => 3u32,
						},
						|_| 1u32,
					) {
						if c < min_cost {
							min_cost = c;
							min_cost_node = *symbol;
						}
					} else {
						eprintln!("cannot find path from {} to {}", header_name, symbol_header);
					}
				}
				if min_cost_node == facade_node {
					eprintln!("cannot find symbol {used}");
					continue;
				}
				if let Some(sym_header) = self.graph.get_symbol_header(min_cost_node) {
					self.graph.graph.add_edge(
						facade_node,
						min_cost_node,
						Edge::ExportsExplicit { source_module: sym_header },
					);
				}
			} else {
				// not found this symbol for all namespaces
			}
		}
	}

	pub fn resolve_exports(graph: &mut Graph<Node, Edge>, facade_node: NodeIndex) {
		// 1. 获取顶层依赖并按 order 排序
		let mut top_level_includes = graph
			.edges(facade_node)
			.filter_map(|edge| {
				if let Edge::Include { order } = edge.weight() {
					Some((*order, edge.target()))
				} else {
					None
				}
			})
			.collect::<Vec<_>>();
		// skip not include
		if top_level_includes.is_empty() {
			return;
		}

		top_level_includes.sort_by_key(|k| k.0);
		// 这个集合现在跟踪已经被更高优先级声明的符号名
		let mut claimed_symbol_names: HashSet<CompactString> = HashSet::new();
		let mut claimed_symbol_idx: HashSet<NodeIndex> = HashSet::new();
		// 2. 依次处理每个顶层依赖链
		for (order, top_level_module_idx) in top_level_includes {
			let is_wildcard_candidate = order == 1;

			// 获取此依赖链提供的所有符号
			let provided_symbols = Self::get_all_provided_symbols(graph, top_level_module_idx);

			// 获取顶层模块的 Rust 模块名，用于生成 `pub use` 语句
			let top_level_rust_name = if let Node::Header(m) = &graph[top_level_module_idx] {
				m.clone()
			} else {
				continue; // 应该不会发生
			};
			// first include
			if is_wildcard_candidate {
				claimed_symbol_idx.extend(&provided_symbols);

				for symbol_idx in provided_symbols.iter() {
					if let Node::Symbol(symbol) = &graph[*symbol_idx].clone() {
						claimed_symbol_names.insert(symbol.clone());
					}
				}
				graph.add_edge(facade_node, top_level_module_idx, Edge::ExportGlob);
				continue;
			} else if provided_symbols.difference(&claimed_symbol_idx).all(|symbol_idx| {
				if let Node::Symbol(symbol) = &graph[*symbol_idx] {
					!claimed_symbol_names.contains(symbol)
				} else {
					true
				}
			}) {
				// no conflict
				claimed_symbol_idx.extend(&provided_symbols);
				for symbol_idx in provided_symbols.iter() {
					if let Node::Symbol(symbol) = &graph[*symbol_idx].clone() {
						claimed_symbol_names.insert(symbol.clone());
					}
				}
				graph.add_edge(facade_node, top_level_module_idx, Edge::ExportGlob);
				continue;
			}

			// has conflict
			let mut difference_nodes: Vec<_> = Vec::new();
			for symbol_idx in provided_symbols.difference(&claimed_symbol_idx) {
				if let Node::Symbol(symbol) = &graph[*symbol_idx].clone() {
					// .clone() to avoid borrow issues

					// 检查此符号是否已经被更高优先级的头文件声明
					if !claimed_symbol_names.contains(symbol) {
						claimed_symbol_names.insert(symbol.clone());
						difference_nodes.push(*symbol_idx);
						graph.add_edge(
							facade_node,
							*symbol_idx,
							Edge::ExportsExplicit {
								// source_module 应该是顶层模块的路径，因为 `pub use` 是针对它的
								source_module: top_level_rust_name.clone(),
							},
						);
					}
				}
			}
			if !difference_nodes.is_empty() {
				claimed_symbol_idx.extend(&difference_nodes);
			}
		}
	}

	/// 辅助函数：从一个给定的模块节点开始，找到所有可传递访问到的模块节点。
	fn get_transitive_module_dependencies(graph: &Graph<Node, Edge>, start_node: NodeIndex) -> HashSet<NodeIndex> {
		let mut reachable_modules = HashSet::new();

		// 只沿着 Includes 边的实现
		let mut visited = HashSet::new();
		let mut stack = vec![start_node];
		visited.insert(start_node);

		while let Some(current_idx) = stack.pop() {
			reachable_modules.insert(current_idx);
			for edge in graph.edges(current_idx) {
				if let Edge::Include { .. } = edge.weight() {
					let target_idx = edge.target();
					if !visited.contains(&target_idx) {
						visited.insert(target_idx);
						stack.push(target_idx);
					}
				}
			}
		}

		reachable_modules
	}
	/// 辅助函数：给定一个模块节点，找到它（以及它的传递依赖）提供的所有符号。
	fn get_all_provided_symbols(graph: &Graph<Node, Edge>, start_module_node: NodeIndex) -> HashSet<NodeIndex> {
		let reachable_modules = Self::get_transitive_module_dependencies(graph, start_module_node);
		let mut provided_symbols = HashSet::new();

		for module_idx in reachable_modules {
			for neighbor_idx in graph.neighbors(module_idx) {
				if let Node::Symbol(_) = graph[neighbor_idx] {
					// 确认这条边是 Define/ExportsExplicit 边
					if graph.find_edge(module_idx, neighbor_idx).map_or(false, |edge_id| {
						matches!(graph[edge_id], Edge::Define | Edge::ExportsExplicit { .. })
					}) {
						provided_symbols.insert(neighbor_idx);
					}
				}
			}
		}
		provided_symbols
	}
	fn has_dir(&self, path: &str) -> bool {
		self.mod_map.keys().any(|m| m.starts_with(path))
	}
}

pub fn header_path_relative(path: &str) -> &str {
	let mut new = path;
	new = new.trim_matches('/');
	new = new.trim_end_matches(".h");
	new = new.trim_end_matches(".hpp");
	new
}

pub(crate) fn parse_header_expand(clang: &Clang, header_content: &str) -> io::Result<String> {
	let mut cmd = Command::new(&clang.path);
	cmd.arg("-save-temps")
		.arg("-E")
		.arg("-")
		.stderr(Stdio::null())
		.stdin(Stdio::piped())
		.stdout(Stdio::piped());
	let mut child = cmd.spawn()?;
	let mut stdin = child.stdin.take().expect("Failed to open stdin");

	let content = header_content.to_string();
	std::thread::spawn(move || {
		stdin.write_all(content.as_bytes()).unwrap();
	});

	let preprocessed = child.stdout.take().unwrap();

	let linereader = BufReader::new(preprocessed);

	let mut header_expand = String::with_capacity(4096);
	header_expand.push_str(header_content.as_str());
	for line in linereader.lines() {
		let line = line?;

		// # 1 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 1 3 4
		if !line.starts_with('#') {
			continue;
		}
		if line.trim().len() == 0 {
			continue;
		}
		if line.trim().ends_with("\" 3 4") {
			continue;
		}
		// eprintln!("line {}", line.trim());
		let mut tokens = line[2..].trim().split_whitespace();
		let _line_no = tokens.next().unwrap();
		let header_file = tokens.next().unwrap().trim_matches('"');
		if header_file.starts_with('<') && header_file != "<stdin>" {
			continue;
		}
		header_expand.push_str(line.as_str());
		header_expand.push('\n');
	}
	Ok(header_expand)
}

pub(crate) fn parse_header_modtree(header_expand: &str, includes: &mut Includes) -> io::Result<()> {
	let mut prev_mod: Option<Rc<RefCell<ModuleTree>>> = None;
	for line in header_expand.split('\n') {
		if line.is_empty() {
			continue;
		}
		if !line.starts_with("# ") {
			continue;
		}
		let mut tokens = line[2..].trim().split_whitespace();
		let _line_no = tokens.next().unwrap();
		let header_file = tokens.next().unwrap().trim_matches('"');
		if header_file == "<stdin>" {
			prev_mod = None;
			continue;
		}
		let flags = if let Some(flags) = tokens.next() {
			flags.parse::<u8>().unwrap()
		} else {
			continue;
		};
		if flags >= 3 {
			continue;
		}

		// header_file = header_path_relative(header_file);
		// eprintln!("header_file = {header_file}");
		if flags == 1 {
			if let Some(sub_mod) = includes.mod_map.get(header_file) {
				if let Some(prev_mod_) = prev_mod.clone() {
					let mut includ = prev_mod_.borrow_mut();
					includ.includes.insert(sub_mod.borrow().name.clone());
					prev_mod = Some(sub_mod.clone());
				} else {
					prev_mod = Some(sub_mod.clone());
				}
			} else {
				let name: Rc<str> = Rc::from(header_file);
				let sub_mod = Rc::new(RefCell::new(ModuleTree { name: name.clone(), ..Default::default() }));
				includes.mod_map.insert(name.clone(), sub_mod.clone());
				if let Some(prev) = prev_mod.clone() {
					prev.borrow_mut().includes.insert(name.clone());
				}
				prev_mod = Some(sub_mod.clone());
			}

			includes.mod_name_map.insert(header_file.into(), path_to_mod(header_file).into());

			// includes.mod_tree.insert_mod_path(header_file);
		} else if flags == 2 {
			if !includes.mod_map.contains_key(header_file) {
				let name: Rc<str> = Rc::from(header_file);
				let sub_mod = Rc::new(RefCell::new(ModuleTree { name: name.clone(), ..Default::default() }));
				includes.mod_map.insert(name, sub_mod.clone());
				prev_mod = Some(sub_mod.clone());
			} else {
				prev_mod = includes.mod_map.get(header_file).cloned();
			}
		}
	}
	Ok(())
}

struct BindingCollector {
	pub duplicates: HashSet<Item>,
}

impl<'ast> Visit<'ast> for BindingCollector {
	fn visit_item(&mut self, item: &'ast Item) {
		self.duplicates.get_or_insert(item.clone());

		syn::visit::visit_item(self, item);
	}
}

pub fn dedup_bindings(input: &TokenStream) -> TokenStream {
	let mut collector = BindingCollector { duplicates: HashSet::new() };

	syn::visit::visit_file(&mut collector, &syn::parse_quote!(#input));

	let mut output = quote! {};

	// 添加所有非重复项
	for item in collector.duplicates.iter() {
		output.extend(item.to_token_stream());
	}
	output
}


#[rustfmt::skip]
fn rust_mangle(name: &str) -> Cow<'_, str> {
	if matches!(
                name,
                "abstract" | "alignof" | "as" | "async" | "await" | "become" |
                    "box" | "break" | "const" | "continue" | "crate" | "do" |
                    "dyn" | "else" | "enum" | "extern" | "false" | "final" |
                    "fn" | "for" | "gen" | "if" | "impl" | "in" | "let" | "loop" |
                    "macro" | "match" | "mod" | "move" | "mut" | "offsetof" |
                    "override" | "priv" | "proc" | "pub" | "pure" | "ref" |
                    "return" | "Self" | "self" | "sizeof" | "static" |
                    "struct" | "super" | "trait" | "true" | "try" | "type" | "typeof" |
                    "unsafe" | "unsized" | "use" | "virtual" | "where" |
                    "while" | "yield" | "str" | "bool" | "f32" | "f64" |
                    "usize" | "isize" | "u128" | "i128" | "u64" | "i64" |
                    "u32" | "i32" | "u16" | "i16" | "u8" | "i8"
            )
	{
		let mut s = name.to_owned();
		s.push('_');
		return Cow::Owned(s);
	}else if name.contains('@') ||
		name.contains('?') ||
		name.contains('$') ||
		name.contains('-') ||
		name.contains('.')  {
		// let mut s = name.to_owned();
		let s = name.replace(['@', '?', '$', '-', '.'], "_");
		// s = s.replace('@', "_");
		// s = s.replace('?', "_");
		// s = s.replace('$', "_");
		// s = s.replace('-', "_");
		// s = s.replace('.', "_");
		return Cow::Owned(s);
	}
	Cow::Borrowed(name)
}
/// 生成rust 安全的mod path, 但不解决冲突
fn path_to_mod(path: &str) -> String {
	let new_path = header_path_relative(path);

	let mut s = String::with_capacity(128);
	for sep in new_path.split('/').filter(|s| !s.is_empty()) {
		s.push_str("::");
		if sep.starts_with(|x: char| x.is_numeric()) {
			s.push_str("_");
		}
		s.push_str(rust_mangle(sep).as_str())
	}
	s
}
/// 生成rust 安全的mod 名称对于的文件路径, 但不解决冲突
fn path_to_mod_path(path: &str) -> String {
	let new_path = path;
	let mut s = String::with_capacity(128);
	for sep in new_path.split('/').filter(|s| !s.is_empty()) {
		s.push_str("/");
		if sep.starts_with(|x: char| x.is_numeric()) {
			s.push_str("_");
		}
		s.push_str(rust_mangle(sep).as_str())
	}

	s
}
/// 写入所有mod 到文件
fn dump_sub_mod(root: &Includes, base_path: &PathBuf) -> io::Result<()> {
	for (header_name, header_mod) in &root.mod_map {
		let mut tokens = vec![quote! {
			use crate::__bindgen::*;
		}];
		let header_node = root.graph.module_nodes.get(header_name).unwrap().clone();
		let mut explicit_exports: HashMap<_, HashSet<SymbolName>> = HashMap::new();
		for edge in root.graph.graph.edges(header_node) {
			match edge.weight() {
				Edge::ExportGlob => {
					let target_node = edge.target();

					let target_name = match root.graph.graph[target_node].clone() {
						Node::Header(header) => header,
						_ => continue,
					};
					let mod_path = root.tree_graph.get_safe_mod_name(target_name.as_str());
					let path: syn::Path = syn::parse_str(&format!("crate{}", mod_path)).unwrap();
					tokens.push(quote! {
						pub use #path::*;
					});
					continue;
				}
				Edge::ExportsExplicit { source_module, .. } => {
					// 找到符号的名字

					if let Node::Symbol(symbol) = &root.graph.graph[edge.target()] {
						explicit_exports.entry(source_module.clone()).or_default().insert(symbol.clone());
					}
				}
				_ => {}
			}
		}

		for (included, syms) in explicit_exports {
			let mod_path = root.tree_graph.get_safe_mod_name(&included);
			let mut sorted_symbols: Vec<SymbolName> = syms.into_iter().collect();
			sorted_symbols.sort();
			let pubuse = format!("pub use crate{}::{{{}}};", mod_path, sorted_symbols.join(","));
			let exports: TokenStream = match syn::parse_str(&pubuse) {
				Ok(path) => path,
				Err(e) => {
					panic!("{}: {}", e, pubuse);
				}
			};
			tokens.push(exports);
		}

		tokens.extend_from_slice(&header_mod.borrow().tokens);
		let header_path = PathBuf::from(format!("{}.rs", root.tree_graph.get_safe_mod_path(header_name)));

		let abs_mod_dir = base_path.join(header_path.parent().unwrap());
		if !abs_mod_dir.exists() {
			fs::create_dir_all(&abs_mod_dir)?;
		}
		let abs_file_dir = base_path.join(header_path);
		let end: TokenStream = tokens.into_iter().collect();
		let mut f = File::create(&abs_file_dir)?;
		f.write_all(DISABLE_WARNING_LINTS.as_bytes())?;

		// f.write_all(end.to_string().as_bytes())?;

		f.write_all(prettyplease::unparse(&syn::parse_quote!(#end)).as_bytes())?;
	}

	Ok(())
}


/// 生成所有mod.rs
fn dump_mod_tree(includes: &Includes, base_path: &PathBuf) -> io::Result<()> {
	use petgraph::prelude::NodeIndex;
	use petgraph::visit::EdgeRef;
	use std::collections::VecDeque;

	let tree = &includes.tree_graph;
	let graph = &tree.graph;
	let root = tree.root;

	// Helper closure to get the string name of a node
	let node_name = |idx: NodeIndex| graph[idx].as_str();

	// BFS traversal for directories
	let mut queue = VecDeque::new();
	queue.push_back((root, base_path.clone()));

	while let Some((dir_idx, dir_path)) = queue.pop_front() {
		// Skip the root node for directory creation
		if dir_idx != root {
			fs::create_dir_all(&dir_path)?;
		}
		// Create mod.rs for this directory (except root)
		let is_root = dir_idx == root;
		let mod_rs_path = if is_root {
			// For root, skip mod.rs
			None
		} else {
			Some(dir_path.join("mod.rs"))
		};
		let mut mod_rs_file = if let Some(ref path) = mod_rs_path {
			Some(File::create(path)?)
		} else {
			None
		};

		// For each outgoing edge, handle subdirs and submodules
		for edge in graph.edges(dir_idx) {
			let target = edge.target();
			match edge.weight() {
				TreeEdge::Subdir => {
					// Subdirectory
					let subdir_name = node_name(target);
					if let Some(ref mut file) = mod_rs_file {
						writeln!(file, "pub mod {};", subdir_name)?;
					}
					let subdir_path = dir_path.join(subdir_name);
					queue.push_back((target, subdir_path));
				}
				TreeEdge::SubMod => {
					// Submodule (file)
					let mod_name = node_name(target);
					if let Some(ref mut file) = mod_rs_file {
						writeln!(file, "pub mod {};", mod_name)?;
					}
				}
				TreeEdge::Parent => {
					// Ignore parent links
				}
			}
		}
	}

	Ok(())
}
/// 生成顶层[generate.rs] 与[__bindgen.rs]
fn dump_top_generate(includes: &Includes, base_path: &PathBuf, clang: &Clang) -> io::Result<()> {
	let tree = &includes.tree_graph;
	let graph = &tree.graph;
	let root = tree.root;
	// create generate.rs listing all top-level modules (direct children of root)
	let dest_path = base_path.join("generate.rs");
	let mut generate_data = String::with_capacity(128);
	// Helper closure to get the string name of a node
	let node_name = |idx: NodeIndex| graph[idx].as_str();

	generate_data.push_str("pub mod __bindgen;\n");
	for edge in graph.edges(root) {
		let target = edge.target();
		match edge.weight() {
			TreeEdge::Subdir | TreeEdge::SubMod => {
				let top = node_name(target);
				generate_data.push_str(&format!("pub mod {};", top));
			}
			TreeEdge::Parent => {}
		}
	}

	for p in clang.c_search_paths.as_ref().unwrap() {
		let path = p.to_str().unwrap();
		if !&includes.has_dir(path) {
			continue;
		}
		let mod_path = path_to_mod(path);
		generate_data.push_str("\n#[allow(ambiguous_glob_reexports)]");
		generate_data.push_str(&format!("\npub use crate{}::*;\n", mod_path));
	}
	let mut f = File::create(&dest_path)?;
	f.write_all(generate_data.as_bytes())?;

	// Write __bindgen.rs if needed (unchanged)
	if let Some(global) = includes.mod_map.get("") {
		let bindgen_path = base_path.join("__bindgen.rs");
		let mut bindf = File::create(&bindgen_path)?;
		bindf.write_all(DISABLE_WARNING_LINTS.as_bytes())?;
		let bindgen_tokens = global.borrow().tokens.clone();
		let bindgen_token: TokenStream = bindgen_tokens.into_iter().collect();
		bindf.write_all(prettyplease::unparse(&syn::parse_quote!(#bindgen_token)).as_bytes())?;
	}
	Ok(())
}

/// 导出所有头文件及符号到编译目录
pub fn dump_to_directory(root: &Includes, path: &PathBuf, clang: &Clang) -> io::Result<()> {
	let base_path = path;

	dump_sub_mod(root, &base_path)?;

	dump_mod_tree(root, &base_path)?;

	dump_top_generate(root, &base_path, clang)?;
	Ok(())
}
