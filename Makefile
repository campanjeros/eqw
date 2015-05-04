PROJECT = eqw

include erlang.mk

all:: deps app build-test-dir rel

build-test-dir:
	@erlc -o ebin -pa ebin test/*.erl
