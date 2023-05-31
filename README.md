# yourbones

> **Warning** _Yourbones_ is still _Work In Progress_.

A companion library for writing applications on Tezos (in OCaml). The name is
arguably funny to anyone who speaks French. Indeed, `your bones` can be
translated as `tes os` in the language of **Molière**, which sounds like
`tezos`... hence the giggle.

## Setting up the development environment

Setting up a development environment is quite common. We recommend setting up a
local switch to collect dependencies locally. Here are the commands to enter to
initiate the environment:

```shellsession
opam update
opam switch create . ocaml-base-compiler.5.0.0 --deps-only -y
eval $(opam env)
```

After initializing the switch, you can collect the development and project
dependencies using `make`:

```shellsession
make dev-deps
make deps
```

Now you should be able to easily start contributing to **Yourbones**.

> **Note** If you are not using [GNU/Make](https://www.gnu.org/software/make/)
> (or equivalent), you can refer to the [Makefile](Makefile) and observe the
> `dev-deps` and `deps` rules to get the commands to run.
