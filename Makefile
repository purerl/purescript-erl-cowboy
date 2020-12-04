.PHONY: ci all ps erl

ci: all

all: erl

ps:
	spago  build

erl: ps
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
