default:
	dune build

install:
	dune install

uninstall:
	dune uninstall

doc:
	dune build @doc

clean:
	dune clean

runtest:
	dune runtest

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev --root .

