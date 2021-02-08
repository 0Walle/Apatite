# Apatite

Apatite is an imperative, low level programming language made in Haskell that generates C code.

It was made primarily to learn Haskell, don't expect any consistency.

Nice things in Apatite:
* Defer statement
* Labeled loops
* Slices? (I haven't tested it yet I don't know if it works)

Not nice things:
* No for statement
* No polymorphism of any kind
* I haven't tested it yet I don't know if it works

## Instalation

Just compile it.

## Usage

Don't use it. But if you try I recommend using the file extension `.appt` cause it sounds nice.

## Example Code

```
func fib_loop(int c) int {
	let int a = 1;
	let int b = 1;
	let int temp;
	while c > 0 {
		temp = a;
		a = b;
		b = temp;
		c -= 1;
	}
	return b;
}

func defer_test(bool it_is) int {
	let int x = 0;
	defer x = 3;
	// defers will execute at end of the scope

	if it_is {
		return x;
	}

	return 0;
}

func label_test() {
	let int a = 0;
	let int b = 0;
	defer b = 0;
	label: while true {
		a += 1;
		defer a = 3;

		while true {
			b += 1;

			break label; // this will execute the defered too
		}
	}
}

type Point = struct {
	int x;
	int y;
}

func struct_literal() Point {
	let Point q = struct Point { 4, 5 };
	// this literal is cursed, be careful

	return q;
}

func main() {
	let int a = 4;

	let int loop = fib_loop(a);

	return;
}
```

## License

This project is licensed under the MIT License - see the LICENSE.md file for details