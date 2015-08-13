.PHONY: all reload compile clean

all: compile

compile: rebar
	./rebar get-deps compile

reload: compile
	zsh -c './reload.erl $$(foreach f (src/*.erl); basename $$f .erl; end)'

rebar:
	wget -c http://cloud.github.com/downloads/basho/rebar/rebar
	chmod +x $@

clean: rebar
	./rebar clean
