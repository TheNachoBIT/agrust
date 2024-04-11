fn multiply_by_two(v: isize) -> isize {
	return v * 200000;
}

// Do some crazy sum, yipeee!
fn do_some_crazy_sum(first: isize, second: isize) -> isize {
	return multiply_by_two(first) + multiply_by_two(second);
}

fn main() -> isize {

	let res: i32 = multiply_by_two(20) as i32;

	return res as isize;
}