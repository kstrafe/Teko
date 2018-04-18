/// Environment used by the implementation.
///
/// The environment represents the Teko environment. It is the environment of a single thread.
/// The environment represents a key-value store, with the values actually being a list of values. Acting as a MultiMap.
/// When a key is used to access a value, the last value in the list associated with the key is used. We can query the environment to push or pop a list.

use std::{collections::HashMap,
          hash::Hash,
          mem,
          result::Result,
          slice,
          vec};

#[derive(Default)]
pub struct Env<K: Eq + Hash, V: Default> {
	/// Arguments to the next call are stored here.
	arguments: Vec<Vec<V>>,
	/// The result of the last computation
	result: V,
	/// The (key list(value)) store
	store: HashMap<K, Vec<V>>,
}

impl<K: Eq + Hash, V: Default> Env<K, V> {
	// Arguments block ////////////////////////////////////
	/// Moves the result to the arguments list
	///
	/// This method resets the result.
	pub fn push_arguments(&mut self) -> Result<(), ()> {
		if let Some(vector) = self.arguments.last_mut() {
			let value = mem::replace(&mut self.result, V::default());
			vector.push(value);
			Ok(())
		} else {
			Err(())
		}
	}

	/// Push a new argument stack
	pub fn push_arguments_stack(&mut self) {
		self.arguments.push(Vec::with_capacity(10));
	}

	/// Pops all arguments and returns an owning iterator over them
	pub fn pop_arguments_stack(&mut self) -> Option<vec::IntoIter<V>> {
		self.arguments.pop().map(|x| x.into_iter())
	}


	/// Pops all arguments and returns a referencing iterator over them
	pub fn get_arguments(&self) -> Option<slice::Iter<V>> {
		self.arguments.last().map(|x| x.iter())
	}

	/// Count the total number of arguments on the current stack
	pub fn count_arguments(&self) -> Option<usize> {
		self.arguments.last().map(|x| x.len())
	}

	// Result block ///////////////////////////////////////
	/// Set the result value in the environment
	pub fn set_result(&mut self, value: V) -> V {
		mem::replace(&mut self.result, value)
	}

	/// Fet the result value in the environment
	///
	/// Fet means to get the result and reset it to its default value.
	/// This ensures not only unique ownership of the result but also
	/// provides us with somewhat of a guarantee that the result is
	/// used once. Doing it this way in Rust makes sense, as we can
	/// use std::mem::replace, which should be very fast.
	pub fn fet_result(&mut self) -> V {
		mem::replace(&mut self.result, V::default())
	}

	/// Gets a reference to the current result
	pub fn get_result(&self) -> &V {
		&self.result
	}

	// Store block ////////////////////////////////////////
	/// Push a key-value pair onto the multimap
	pub fn push(&mut self, key: K, value: V) {
		let vector = self.store.entry(key).or_insert_with(|| vec![]);
		vector.push(value);
	}

	/// Pop the top value associated with they key
	pub fn pop(&mut self, key: &K) -> Option<V> {
		let (remove, value) = if let Some(vector) = self.store.get_mut(key) {
			let value = vector.pop();
			(vector.len() == 0, value)
		} else {
			(false, None)
		};
		if remove {
			self.store.remove(key);
		}
		value
	}

	/// Set the top value associated with the key
	///
	/// If the key does not exist, nothing is done.
	pub fn set(&mut self, key: &K, value: V) -> Result<(), ()> {
		if let Some(vector) = self.store.get_mut(key) {
			vector.push(value);
			Ok(())
		} else {
			Err(())
		}
	}


	/// Get a reference to the current top value associated with the key
	pub fn get(&self, key: &K) -> Option<&V> {
		if let Some(vector) = self.store.get(key) {
			vector.last()
		} else {
			None
		}
	}

	/// Check if an entry exists in the environment
	pub fn exists(&self, key: &K) -> bool {
		self.store.contains_key(key)
	}

	/// Count the number of total values stored
	///
	/// This includes the values in each list.
	pub fn count(&self) -> usize {
		let mut sum = 0;
		for (_, value) in self.store.iter() {
			sum += value.len()
		}
		sum
	}

	/// An iterator to all keys in the store
	pub fn keys(&self) -> vec::IntoIter<&K> {
		self.store.iter()
		          .map(|(k, _)| k)
		          .collect::<Vec<&K>>()
		          .into_iter()

	}
}

// TODO Implement more tests

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn test_exists_0() {
		let mut env = Env::<&str, usize>::default();
		assert![!env.exists(&"key")];
	}
	#[test]
	fn test_exists_1() {
		let mut env = Env::default();
		env.push("key", 0);
		assert![env.exists(&"key")];
	}
	#[test]
	fn test_exists_2() {
		let mut env = Env::default();
		env.push("key", 0);
		env.push("key", 1);
		assert![env.exists(&"key")];
	}
	#[test]
	fn test_exists_3() {
		let mut env = Env::default();
		env.push("key", 0);
		env.push("key", 1);
		env.push("key 2", 1);
		assert![env.exists(&"key")];
		assert![env.exists(&"key 2")];
	}
	#[test]
	fn test_exists_4() {
		let mut env = Env::default();
		env.push("key", 0);
		env.push("key", 1);
		env.push("key 2", 1);
		env.pop(&"key");
		assert![env.exists(&"key")];
		assert![env.exists(&"key 2")];
	}
	#[test]
	fn test_exists_5() {
		let mut env = Env::default();
		env.push("key", 0);
		env.push("key", 1);
		env.push("key 2", 1);
		env.pop(&"key");
		env.pop(&"key");
		assert![!env.exists(&"key")];
		assert![env.exists(&"key 2")];
	}
	#[test]
	fn pushing_and_popping() {
		let mut env = Env::default();
		env.push("a", 0);
		env.push("b", 1);
		assert_eq![env.pop(&"a"), Some(0)];
		assert_eq![env.pop(&"b"), Some(1)];
		assert_eq![env.pop(&"a"), None];
	}
}
