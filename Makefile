.PHONY: all test build clean check-lint lint doc utop
.PHONY: pinned-deps dev-deps local-deps

all: build

# [make build] (or [make]) will build the source
build:
	dune build

# [make test] run all test
test:
	dune runtest --no-buffer -j 1

# [make clean] remove build artifacts (essentially the
# "_build/" folder)
clean:
	dune clean

# [make doc] build the documentation (using odoc)
doc: build
	dune build @doc

# [make utop] launch an REPL with the whole package in
# the context
utop:
	dune utop

# [make check-lint] ensure that the code is properly formatted
# according to the ".ocamlformat" file
check-lint:
	dune build @fmt

# [make lint] apply formatting according to the ".ocamlformat" file
lint:
	dune build @fmt --auto-promote



# Setting up the development environment

.PHONY: dev-deps deps

# [make dev-deps] will download locally the dependencies needed
# to develop the project. Mainly formatting features, and IDE support.
dev-deps:
	opam install dune merlin ocamlformat ocp-indent utop -y

# [make local-deps] will download locally the dependencies needed
# to build the libraries. That is, all the dependencies referenced
# in the OPAM description files.
local-deps:
	opam install . --deps-only --with-doc --with-test -y

# [make pinned-deps] will download locally the pinned dependencies needed.
pinned-deps: local-deps
	opam pin add nightmare --dev-repo git+https://github.com/funkywork/nightmare.git -y

# [make deps] will fetch the local and the pinned deps.
deps: pinned-deps
