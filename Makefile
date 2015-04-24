PROJECT = eqw

DEPS = erlcloud

dep_erlcloud = git git://github.com/campanja-forks/erlcloud.git \
	campanja-20150212

AUTOPATCH += meck

include erlang.mk

all:: deps app build-test-dir rel

build-test-dir:
	@erlc -o ebin -pa ebin test/*.erl
