.PHONY: all compile xref dialyzer test

all: xref

compile:
	@rebar3 compile

xref:
	@rebar3 xref

release:
	@rebar3 release

dialyzer:
	@rebar3 dialyzer

test: xref
	@rebar3 eunit

clean:
	@rebar3 clean

distclean: clean
	@rm -rfv _build

shell: xref
	@erl -pa _build/default/lib/*/ebin
