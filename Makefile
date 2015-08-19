REBAR=$(shell which rebar || echo ./rebar)
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

all: deps compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://raw.github.com/wiki/rebar/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

libvirt_constants:
	if [ -z "$$VERX_LIBVIRT_INCLUDE" ]; then VERX_LIBVIRT_INCLUDE="priv/include"; fi; \
	bash -o pipefail -c "bin/mk_libvirt_constants $$VERX_LIBVIRT_INCLUDE/*.h | sort | uniq > include/libvirt_constants.hrl"

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) check-deps || $(REBAR) get-deps

test: $(REBAR) compile
	@$(REBAR) xref eunit recursive=false

.PHONY: test dialyzer typer clean distclean tags

$(DEPSOLVER_PLT):
	@dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto

dialyzer: $(DEPSOLVER_PLT)
	@dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src test

typer: $(DEPSOLVER_PLT)
	@typer --plt $(DEPSOLVER_PLT) -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)

tags:
	find . -name "*.[he]rl" -print | etags -

