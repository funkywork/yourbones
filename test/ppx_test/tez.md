# Literal definition of `tez` and `mutez`

The operations on `tez` (and by extension, `mute`) try to be as safe as
possible. This implies that their creation usually wraps the result in the
`result` type. It can be annoying when you declare literal values, which you
know are valid. That's why `yourbones-ppx` provides a very slight syntax
extension to describe `tez`, literally, which will be checked at compile time.
Indeed, one can declare integers, with the suffixes `m` and `t` describing
respectively the production of `mutez` or the production of `tez`.

```ocaml
# 120t ;;
- : tez = 120.000000ꜩ
```

```ocaml
# 120m ;;
- : tez = 0.000120ꜩ
```

You can use literal expression like that:

```ocaml
# 120m < 120t ;;
- : bool = true
```

However, as the conversion to `tez` is done at compile time, it is still
impossible to describe negative amounts or too large amounts!

```ocaml
# -123m ;;
Line 1, characters 1-6:
Error: "-123" projection into mutez fails with `Tez_negative_amount (-123)
```

```ocaml
# 9223372036854775807t
Line 1, characters 1-21:
Error: "9223372036854775807" projection into tez fails with `Tez_overflow
```

In general, literal values are used to define constants, especially for
configuration.
