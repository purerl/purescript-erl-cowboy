.PHONY: ps erl all test

all: erl

test: erl

ps:
	spago  build

erl: ps
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
