REBAR := rebar3

all: compile

compile:
	@$(REBAR) compile

libvirt_constants:
	bash -o pipefail -c "bin/mk_libvirt_constants $${VERX_LIBVIRT_INCLUDE-priv/include}/*.h | sort | uniq > include/libvirt_constants.hrl"

clean:
	@$(REBAR) clean

test: compile
	@$(REBAR) xref eunit recursive=false

.PHONY: test dialyzer typer clean distclean tags

dialyzer:
	$(REBAR) dialyzer

typer: $(DEPSOLVER_PLT)
	@typer --plt $(DEPSOLVER_PLT) -r ./src

tags:
	find . -name "*.[he]rl" -print | etags -
