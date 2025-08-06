
mod modtree;
mod target_env;

#[cfg(target_os = "linux")]
mod linux;

use crate::modtree::{dump_to_directory, parse_header_expand, parse_header_modtree, Includes};
use bindgen;
use bindgen::callbacks::{EnumVariantCustomBehavior, EnumVariantValue, IntKind, MacroParsingBehavior, Token};
use compact_str::CompactString;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use crate::linux::get_header_dependencies;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, RwLock};
use std::{env, io};

use rayon::prelude::*;

#[derive(Debug, Default, Clone)]
pub struct ParseCallbacks {
	marco_syms: Arc<RwLock<HashSet<CompactString>>>,
	enum_syms: Arc<RwLock<HashSet<CompactString>>>,
}

impl bindgen::callbacks::ParseCallbacks for ParseCallbacks {
	fn will_parse_macro(&self, _name: &str) -> MacroParsingBehavior {
		if self.enum_syms.read().unwrap().contains(_name) {
			eprintln!("ignore same name macro {}", _name);
			MacroParsingBehavior::Ignore
		} else {
			if !self.marco_syms.read().unwrap().contains(_name) {
				self.marco_syms.write().unwrap().insert(_name.into());
			}
			MacroParsingBehavior::Default
		}
	}
	fn modify_macro(&self, _name: &str, _tokens: &mut Vec<Token>) {

	}
	fn int_macro(&self, _name: &str, _value: i64) -> Option<IntKind> {

		None
	}
	fn str_macro(&self, _name: &str, _value: &[u8]) {

	}
	fn func_macro(&self, _name: &str, _value: &[&[u8]]) {

	}
	fn enum_variant_behavior(
		&self,
		_enum_name: Option<&str>,
		_original_variant_name: &str,
		_variant_value: EnumVariantValue,
	) -> Option<EnumVariantCustomBehavior> {
		if self.marco_syms.read().unwrap().contains(_original_variant_name) {
			return Some(EnumVariantCustomBehavior::Hide);
		}
		if !self.enum_syms.read().unwrap().contains(_original_variant_name) {
			self.enum_syms.write().unwrap().insert(CompactString::from(_original_variant_name));
		}

		Some(EnumVariantCustomBehavior::ModuleConstify)
	}
}

fn main() {
	let mut root = Includes::default();

	let cb = ParseCallbacks::default();
	let clang = clang_sys::support::Clang::find(None, &[])
		.ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Cannot find clang executable"))
		.unwrap();

	let search_paths = clang.c_search_paths.as_ref().unwrap().iter().map(|x| x.clone()).collect::<Vec<_>>();

	root.mod_map.insert("".into(), Rc::new(RefCell::new(Default::default())));
	let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
	let mut headers: Vec<&str> = Vec::new();
	#[cfg(target_env = "gnu")]
	headers.extend(target_env::GLIBC_HEADERS);
	#[cfg(target_env = "musl")]
	headers.extend(target_env::MUSL_HEADERS);

	#[cfg(target_os = "linux")]
	headers.extend(linux::LINUX_HEADERS);

	let header_depends = get_header_dependencies();


	let mut header_contents = Vec::with_capacity(headers.len());

	for header in headers.iter() {
		if !search_paths.iter().any(|path| path.join(header).is_file()) {
			continue;
		}

		let content = if header_depends.contains_key(header) {
			let mut content = String::with_capacity(128);
			let depend = header_depends.get(header).unwrap();
			for h in depend.iter() {
				content.push_str(&format!("#include <{}>\n", h));
			}
			content.push_str(&format!("#include <{}>\n", header));

			content
		} else {
			format!("#include <{header}>\n")
		};
		header_contents.push(content);
	}

	let header_expands = header_contents
		.par_iter()
		.filter_map(|content| match parse_header_expand(&clang, content) {
			Ok(result) => Some(result),
			Err(_) => None,
		})
		.collect::<Vec<_>>();

	for expand in header_expands.into_iter() {
		parse_header_modtree(expand.as_str(), &mut root).unwrap();
	}

	let bindings_ = header_contents
		.par_iter()
		.filter_map(|content| {
			let bindings = match bindgen::Builder::default()
				.header_contents("wrapper.h", content.as_str())
				.parse_callbacks(Box::new(cb.clone()))
				.prepend_enum_name(false)
				.hash_unnamed_enum()
				.use_core()
				.layout_tests(false)
				.enable_split_to_file()
				.formatter(bindgen::Formatter::None)
				.generate()
			{
				Ok(bindings) => {
					let mut tokens = HashMap::new();
					bindings.token_stream_by_header(|path, token| {
						tokens.insert(path.to_string(), token.to_string());
					});
					Some(tokens)
				}
				Err(e) => {
					eprintln!("Cannot generate bindings: {}", e);
					None
				}
			};
			bindings
		})
		.collect::<Vec<_>>();

	for binding in bindings_.into_iter() {
		for (path, token_string) in binding {
			if let Some(cur_mod) = root.mod_map.get(path.as_str()) {
				// 跳过已解析头文件符号,
				if token_string.is_empty() {
					continue;
				}

				let tokens = syn::parse_str::<proc_macro2::TokenStream>(&token_string).unwrap();
				cur_mod.borrow_mut().tokens.push(tokens.clone())
			}
		}
	}

	root.global_mod_dedup();
	root.parse_mod_sym();
	root.parse_treegraph();
	root.parse_graph();

	dump_to_directory(&root, &out_dir, &clang).expect("dump mods failed");
}
