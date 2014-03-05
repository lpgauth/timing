PROJECT=timing
REBAR=./rebar

all: deps compile

clean:
	@rm -rf deps
	@$(REBAR) clean

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar get-deps..."
	@$(REBAR) update-deps

.PHONY: deps
