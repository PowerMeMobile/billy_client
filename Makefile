NAME=billy_client
OTP_PLT=~/.otp.plt
PRJ_PLT=$(NAME).plt

all: compile

compile: get-deps
	@./rebar compile

compile-fast:
	@./rebar compile

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

clean:
	@./rebar clean

dialyze: $(OTP_PLT) compile $(PRJ_PLT)
	@dialyzer --plt $(PRJ_PLT) -r ./ebin

$(OTP_PLT):
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps erts \
		kernel stdlib crypto mnesia sasl common_test eunit ssl \
		asn1 compiler syntax_tools inets

$(PRJ_PLT):
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(PRJ_PLT) \
	-r ./deps/*/ebin

console:
	./console.sh

develop:
	./develop.sh

tags:
	find . -name "*.[e,h]rl" -print | etags -
