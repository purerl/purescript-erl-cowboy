.PHONY: ps erl all test

all: ps erl

test: erl

ps:
	spago  build

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
