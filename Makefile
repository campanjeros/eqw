REBAR="rebar"

.PHONY: all compile check test doc clean get-deps update-deps

all: get-deps compile build-test-dir xref

compile:
	@$(REBAR) -j 12 compile

xref:
	@$(REBAR) -j 12 skip_deps=true xref

clientd.plt:
	dialyzer --output_plt clientd.plt --build_plt \
		--apps stdlib kernel inets log4erl lhttpc

dialyzer: get-deps compile
	dialyzer --plt clientd.plt --src src

test: all
	@$(REBAR) -j ct skip_deps=true

external-test: all
	@rm -rf .eunit apps/*/.eunit
	@$(REBAR) -j eunit skip_deps=true -DEUNIT_EXTERNAL

doc:
	@$(REBAR) -j doc skip_deps=true

clean:
	@rm -rf test/*.beam
	@$(REBAR) -j clean

dist-clean: clean
	@$(REBAR) -j delete-deps

get-deps:
	@$(REBAR) -j get-deps

update-deps:
	@$(REBAR) -j update-deps
	@$(REBAR) -j get-deps

build-test-dir: compile
	@erlc -o ebin -pa ebin test/*.erl
