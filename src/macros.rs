/// Macro to construct the library table
macro_rules! construct_builtins {
	({$($c:expr => $x:expr),*,} $($t:ident: $e:expr => $i:ident),*,) => {
		{
			let mut functions_and_macros : HashMap<Symbol, Program> = [
				$(
					($e.into(), vec![Arc::new(Sourcedata(None, Coredata::$t($t::Builtin($i, $e.into()))))])
				),*
			].iter().cloned().collect();
			let constants : HashMap<Symbol, Program> = [
				$(
					($c.into(), vec![Arc::new(Sourcedata(None, $x))])
				),*
			].iter().cloned().collect();
			functions_and_macros.extend(constants);
			functions_and_macros
		}
	};
}

/// Allows us to specify externs as a list instead of tediously repeating extern crate
macro_rules! externs {
	($($i:ident)*) => {
		$(
			extern crate $i;
		)*
	};
}

/// Same as externs but for pub mod
macro_rules! pubmods {
	($($i:ident)*) => {
		$(
			pub mod $i;
		)*
	};
}
