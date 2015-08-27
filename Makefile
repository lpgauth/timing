PROJECT=timing
REBAR=./rebar

all: deps compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps erts kernel stdlib deps/*/ebin

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@rm -rf deps
	@$(REBAR) clean

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar get-deps..."
	@$(REBAR) update-deps

dialyzer:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt

.PHONY: clean compile deps
