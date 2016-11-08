.PHONY: clean distclean upgrade compile test dialyzer eunit xref

default: compile

clean:
	./rebar3 clean
	rm -f _build/*/lib/*/ebin/*
	find . -name "erlcinfo" -exec rm {} \;

distclean: clean
	rm -rf _build
	rm -f rebar.lock
	rm -rf .release

upgrade:
	./rebar3 upgrade

compile:
	./rebar3 compile

test: xref eunit dialyzer

dialyzer:
	./rebar3 dialyzer

eunit:
	./rebar3 eunit

xref:
	./rebar3 xref
