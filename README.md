# FlowLang
[![Rust](https://github.com/luizgabriel/FlowLang/actions/workflows/rust.yml/badge.svg?branch=main)](https://github.com/luizgabriel/FlowLang/actions/workflows/rust.yml)

[WIP] A statically typed, functional programming language.

## Example Syntax:

### Function Definition & Application

```hs
square x = x * x
hypotenuse cat1 cat2 = square x + square y

hypotenuse 3 4
```

### Operator Functions Definitions & Application

```hs
(**) x y = hypotenuse x y
3 + (5 ** 6) / 2
```

### Piping Operator

```hs
add5 = (+) 5
times2 = (*) 2

7 |> times2 |> add5
```

### If Expressions

```hs
max x y = if x > y then x else y
min x y = if x < y then x else y

max 5 -7
min 6.8 2.7
```

### Lambda Expressions

```hs
add1 = x -> x + 1
add1 5
```

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
