.PHONY: all

PREFIX ?= '/usr/local'

all:
	@ echo "Building escript..."
	@ rebar3 escriptize

.PHONY: install
install: all
	@ echo "Installing escript..."
	@ mkdir -p '${PREFIX}/bin'
	@ cp _build/default/bin/els_dap ${PREFIX}/bin

.PHONY: clean
clean:
	@rm -rf _build

$HOME/.dialyzer_plt:
	dialyzer --build_plt --apps erts kernel stdlib

ci: $HOME/.dialyzer_plt
	rebar3 do compile, ct, dialyzer, xref, cover, edoc
