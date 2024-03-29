# Literal definition of `address`

Even if addresses can be considered as simple strings, they must have a prefix,
a fixed size and a valid checksum. As with `tez`, this involves wrapping an
address in a `result`. There are therefore extensions that allow you to declare literal addresses for `tz1`, `tz2`, `tz3` and `KT1`.

```ocaml
# let an_address = [%tz1 "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL"] ;;
val an_address : Address.t = tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL
```

```ocaml
# let a_contract_address = [%kt1 "KT1RvwLgpxVv9ANCKsDb5vBgTaZRG1W4bKWP"] ;;
val a_contract_address : Address.t = KT1RvwLgpxVv9ANCKsDb5vBgTaZRG1W4bKWP
```

And the description of an address is validated at compile time, so it's
impossible to construct an invalid address:

```ocaml
# let invalid_address = [%tz2 "an invalid address"]
Line 1, characters 23-50:
Error: "an invalid address" projection into a "tz2" address fails with
       `Address_invalid_length ("an invalid address")
```

And we can also use a generic projection to an address:

```ocaml
# let an_arbitrary_address = [%address "tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL"] ;;
val an_arbitrary_address : Address.t = tz1XxRjkB77R1rnxVRGQxmWAcG1cGQQeMAAL
```
