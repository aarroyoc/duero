# Duero - a toy Strand interpreter

**Duero** is a _toy_ Strand interpreter. Strand is a programming language from the late 80's, part of the concurrent logic programming paradigm. Think of it as a Prolog cousin, in which everything executes in parallel. It's made in Rust and uses Tokio.

This implementation does not try to be complete, it's just a toy to experiment. [FLENG](https://www.call-with-current-continuation.org/fleng/fleng.html) is a more mature and complete option.

# Examples

## Fibonacci

```
fib(N, X) :- N =:= 1 | X := 1.
fib(N, X) :- N =:= 2 | X := 1.
fib(N, X) :- X is X0 + X1, fib(N0, X0), fib(N1, X1), N0 is N - 2, N1 is N - 1.

main :- fib(12, X), print(X).
```

This program calculates the fibonacci number 12. It does it in parallel, as you can see that the order of each function call does not matter to compute the end result.

## Producer - Consumer

```
producer(Stream) :- Stream := a(5, Stream1), producer(Stream1).
consumer(a(N, Stream)) :- print(N), consumer(Stream).

main :- producer(Stream), consumer(Stream).
```

This program has a recursive producer (produces 5) and a recursive consumer (prints it). They work in parallel to create an endless loop.
