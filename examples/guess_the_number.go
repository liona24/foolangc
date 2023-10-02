func print(int ptr, int size) {
	@syscall(1, 1, ptr, size, 0, 0, 0);
}
func read(int ptr, int size) {
	@syscall(0, 0, ptr, size, 0, 0, 0);
}

func read_number() = int {
	byte[10] digits = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";

	int i = 0;
	loop {
		read(&digits + i, 1);
		if digits[i] < \48 or not (digits[i] < \58) {
			break;
		}

		i = i + 1;
		if not (i < 10) {
			break;
		}
	}

	int factor = 1;
	int result = 0;
	loop {
		if i == 0 {
			break;
		}
		i = i - 1;

		result = result + factor * (digits[i] - \48);
		factor = factor * 10;
	}

	return result;
}

byte[18] prompt = "give me a number: ";
byte[23] win = "you guessed correctly!\x0a";
byte[14] fail = "very wrong :(\x0a";

print(&prompt, 18);
int number = read_number();

if number == 0x1337 {
	print(&win, 23);
	@exit(0);
} else {
	print(&fail, 14);
	@exit(1);
}
