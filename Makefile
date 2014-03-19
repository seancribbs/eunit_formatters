.PHONY: deps test

all: compile

compile:
	./rebar compile

# deps:
# 	./rebar get-deps

clean:
	./rebar clean

DIALYZER_APPS = kernel stdlib sasl erts eunit

include tools.mk
