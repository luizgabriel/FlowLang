# FlowLang

[WIP] A statically typed, functional programming language.

## Getting Started

Install the **Rust** programming language:

> [https://www.rust-lang.org/pt-BR/learn/get-started](https://www.rust-lang.org/pt-BR/learn/get-started)

Clone this repository:

```
git clone git@github.com:luizgabriel/FlowLang.git
cd FlowLang
```

Run the REPL (Read Evaluate Parse Loop) with:

```sh
cargo run --bin repl -q
```

Example Syntax:

```hs
square x = x * x
hypotenuse cat1 cat2 = square x + square y

hypotenuse 3 4

add_five = (+) 5
add_five 10

(**) x y = hypotenuse x y
3 + (5 ** 6) / 2
```
