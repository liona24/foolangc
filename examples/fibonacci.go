func print(int ptr, int size) {
	@syscall(1, 1, ptr, size, 0, 0, 0);
}

func fib(int n) = int {
	if n == 0 {
		return 0;
	} elif n == 1 {
		return 1;
	} else {
		return fib(n - 1) + fib(n - 2);
	}
}

func print_number(int n) {
	byte[16] digits = "0123456789ABCDEF";
	byte[8] bytes = n;

	int i = 7;
	loop {
		# some hacks because we do not have div or any bit shift
		byte[8] tmp = 16 * bytes[i];
		int upper = tmp[1];
		tmp = 16 * tmp[0];
		int lower = tmp[1];

		print(&digits + upper, 1);
		print(&digits + lower, 1);

		i = i - 1;
		if i < 0 {
			break;
		}
	}

	print(&"\x0a", 1);
}

int x = fib(17);
print_number(x);
