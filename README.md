# My solution to <https://tdpc.contest.atcoder.jp/tasks/tdpc_fibonacci>

Build with:

```sh
$ git clone https://github.com/minoki/typical-dp-contest-t-fibonacci.git
$ cd typical-dp-contest-t-fibonacci
$ stack build
```

Note: Uncomment `- -fllvm` in package.yaml if you have LLVM available.

Print the sequence:

```sh
$ echo 2 10 | stack exec atcoder-fib-exe List
[1,1,2,3,5,8,13,21,34,55]
$ echo 3 10 | stack exec atcoder-fib-exe List
[1,1,1,3,5,9,17,31,57,105]
```

Specify the method to get the answer:

```sh
$ echo 2 10 | stack exec atcoder-fib-exe NaiveIteration
55
$ echo 3 10 | stack exec atcoder-fib-exe NaiveIteration
105
$ echo 1000 1000000000 | stack exec time atcoder-fib-exe PolyMulUnboxed
289587781
        1.46 real         1.43 user         0.03 sys
```

Available methods are:

- `NaiveIteration`
    - Uses the recurrence relation.
- `MatMul`
    - Uses K&times;K matrix multiplication.
- `MatMulUnboxed`
    - `MatMul` with unboxed arrays.
- `PolyMul`
    - Uses polynomial division with remainder.
- `PolyMulUnboxed`
    - `PolyMul` with unboxed vectors.
- `FastDoubling`
    - "Fast doubling" method.
- `FastDoublingMut`
    - "Fast doubling" method with mutable vectors.
