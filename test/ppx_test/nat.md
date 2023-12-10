# Literal definition of `nat`

As with `tez` (and `mutez`), it is possible to use a literal suffix to describe
a natural number, ensuring that it is positive, by using the suffix `N`.


```ocaml
# 127N ;;
- : nat = 127
```

This makes it impossible to express negative natural numbers:

```ocaml
# -234354N ;;
Line 1, characters 1-9:
Error: "-234354" projection into Natural impossible
```

This makes it possible to describe arithmetic operations in a fairly concise
way:

```ocaml
# Nat.((1N + 223N * 1345N) -^ 12N) ;;
- : nat = 299924
```
