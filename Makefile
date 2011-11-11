all: generate

deps:
	@./rebar get-deps

clean:
	@./rebar clean

dialyze:
	@./rebar clean
	@./rebar compile debug_info=1
	@./rebar dialyze skip_deps=true
	@./rebar clean

build-plt:
	@./rebar build-plt skip_deps=true

compile: deps
	@./rebar compile

generate: compile
	@rm -rf ./rel/billy_client
	@./rebar generate

compile-fast: 
	@./rebar compile

generate-fast: compile-fast
	@rm -rf ./rel/billy
	@./rebar generate


console:
	./rel/billy_client/bin/billy_client console

release: generate
	./rel/create-release.sh

