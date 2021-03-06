.PHONY: all compile libvirt_constants test dialyzer typer tags

REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

libvirt_constants:
	bin/mk_libvirt_constants $${VERX_LIBVIRT_INCLUDE-priv/include}/*.h | sort | uniq > include/libvirt_constants.hrl

clean:
	@$(REBAR) clean

test: compile
	@$(REBAR) xref eunit recursive=false

dialyzer:
	$(REBAR) dialyzer

typer:
	@typer \
		-pa _build/default/lib/verx/ebin \
		-I include \
		--plt _build/default/*_plt \
		-r ./src
tags:
	find . -name "*.[he]rl" -print | etags -
