use std::{cmp, fmt};

#[derive(Debug)]
pub enum Userdata {
}

pub fn user_data_name(data: &Userdata) -> &str {
	""
}

impl cmp::PartialEq for Userdata {
	fn eq(&self, other: &Self) -> bool {
		true
	}
}

impl fmt::Display for Userdata {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write![f, ""]
	}
}

