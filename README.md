# ron.el - Rust Object Notation parser 

This is a library for parsing RON (Rust Object Notation).

The details of this implementation are based on the following
description of [RON](https://github.com/ron-rs/ron/blob/master/docs/grammar.md).

## Features

It can parse successfully parse most RON objects as defined in the grammar above.

It has a few exceptions currently:
- Not all identifiers are supported.
- Bytes are not supported.

## Example

To get the general idea of what this package does, an example of this package in
use can be found in `example.el`.

## Testing

Testing of the RON parser can be done via the following command:

``` shell
make test
```

## Contributing

Contributions are welcome, either open an issue or a pull request. Any pull
requests regarding the parser functionality: I would suggest that it is paired
with appropriate and exhaustive test cases inside of `test-ron.el`.

## License

This project is released under the `GPL-3.0 license`. See `LICENSE` for more details.


