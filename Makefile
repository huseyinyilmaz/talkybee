# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar
MNESIA_DIR = /tmp/mnesia
NODE_NAME = dev_node

all: compile

compile: # deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .talkybee_dialyzer.plt \
		--apps kernel stdlib deps/*

dialyze:
	@$(DIALYZER) --src lib/*/src --plt .talkybee_dialyzer.plt -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

shell: compile
	erl -pa lib/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -mnesia dir $(MNESIA_DIR) \
	    -sname $(NODE_NAME)

start: compile
	erl -pa lib/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -mnesia dir $(MNESIA_DIR) \
	    -sname $(NODE_NAME) \
	    -eval "application:start(sasl),\
                   appmon:start(),\
		   chat:start(),\
		   http:start()."

dev: compile
	erl -pa lib/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -mnesia dir $(MNESIA_DIR) \
	    -sname $(NODE_NAME) \
	    -eval "application:start(sasl),\
                   appmon:start(),\
		   chat:start(),\
		   chat:create_room(1),\
                   {ok, Room} = c_room:get_room(1),\
                   chat:create_user(1,1),\
                   chat:create_user(2,2),\
		   {ok, User1} = c_user:get_user(1),\
                   {ok, User2} = c_user:get_user(2),\
                   c_room:add_user(Room,User1),\
                   c_room:add_user(Room,User2)."