.PHONY: deps test

all: deps compile

compile: deps
	./rebar compile

deps:
	test -d deps || ./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

DIALYZER_APPS = kernel stdlib erts sasl ssl crypto public_key

include tools.mk
