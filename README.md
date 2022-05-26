# Welcome to Kaon

[![Rust](https://github.com/PlutonianHacker/kaon-lang/actions/workflows/rust.yml/badge.svg)](https://github.com/PlutonianHacker/kaon-lang/actions/workflows/rust.yml)

A little scripting language written in rust.

## Examples

### Hello World

A simple hello world program:
```rust
System.println("Hello, World!")
```
### Classes

A slightly more convoluted example using classes:
```javascript
class Vector {
    // A field
    var x = 0
    var y = 0

    // A constructor
    create new(a, b) {
        self.x = a
        self.y = b
    } 

    fun add(other: Vector) {
        self.x = self.x + other.x
        self.y = self.y + other.y
    }

    // A method
    fun to_string() {
        return "{ x: " + self.x + " y: " + self.y + " }"  
    }
}

fun main() {
    var v1 = Vector.new(4, 5)
    var v2 = Vector.new(6, 7)

    v1.add(v2)

    print(v1.to_string()) // -> { x: 10 y: 12 }
}

main()
```

More examples can be found in <a href="scripts/">scripts</a> or <a href="tests/kaon/">tests/kaon</a>.

## Features 

<li>Object-oriented</li>
<li>Bytecode compiler and VM</li>
<li>Simple, straightforward syntax</li>
<li>Strongly typed; the compiler does its best to eliminate runtime errors</li>
<br>

## Getting Started
To get started using kaon, first make sure you have cargo installed, then run the following commands:
```console
git clone https://github.com/PlutonianHacker/kaon-lang.git
cd kaon-lang
cargo build
```
## Usage
Currently the only way to run a Kaon script is with `cargo run` inside the kaon-lang directory. 
To run a file, use `cargo run <FILE.kaon>`, if no file is provided, interactive mode is run instead. To see a list of options, run `cargo run -- --help`.

## Contributing
All contributions are welcome! If you find any bugs or have ideas to make the language better, you can open up an issue. 

## License
Kaon is avaliable under the permissive MIT license. 
