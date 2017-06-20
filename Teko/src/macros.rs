/// Macro to construct the library table
macro_rules! construct_builtins {
	({$($c:expr => $x:expr),*,} $($t:ident: $e:expr => $i:ident),*,) => {
		{
			let mut functions_and_macros : HashMap<String, Program> = [
				$(
					($e.into(), vec![Rc::new(Sourcedata(None, Coredata::$t($t::Builtin($i))))])
				),*
			].iter().cloned().collect();
			let constants : HashMap<String, Program> = [
				$(
					($c.into(), vec![Rc::new(Sourcedata(None, $x))])
				),*
			].iter().cloned().collect();
			functions_and_macros.extend(constants);
			functions_and_macros
		}
	};
}
