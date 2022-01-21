# Welcome to Kaon
A little scripting language written in rust.
# Features 
<li>Bytecode compiler and VM</li>
<li>Simple, straightforward syntax</li>
<li>Strongly typed; the compiler does its best to eliminate runtime errors</li>
<li>Both functional and object oriented</li>

# Examples

## Hello, World
A simple hello world program:
```javascript
println("Hello, World!")
```
## Area of Circle
A slightly more convoluted example:
```javascript
con PI = 3.14159265358979324

var radius = 3
var area = PI * radius.pow(2)

println("Area of circle: " + area.to_string())
```

# Getting Started
To get started using kaon, first make sure you have cargo installed, then run the following commands:
```bash
git clone https://github.com/PlutonianHacker/kaon-lang.git
cd kaon-lang
cargo build
```
## Usage
Currently the only way to run a Kaon script is with `cargo run` inside the kaon-lang directory. 
To run a file, use `cargo run <FILE.kaon>`, if no file is provided, interactive mode is run instead. To see a list of options, run `cargo run -- --help`.

# Hacking
All contributions are welcome! If you find any bugs or have ideas to make the language better, you can open up an issue. 
