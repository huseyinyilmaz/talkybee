DIALYZER = dialyzer
REBAR = ./rebar
MNESIA_DIR = /tmp/mnesia
NODE_NAME = dev_node

# compile
all: compile

# get dependencies
getdeps:
	@$(REBAR) get-deps

# initialize development
initdev: init
	cd devutils; if [ ! -d "sync" ]; then git clone git://github.com/rustyio/sync.git sync; fi
	cd devutils/sync; mkdir -p ebin; make

init: getdeps collectstatic

compile:
	@$(REBAR) compile

compiledev: compile 
	cd devutils/sync; make
	cd devutils/utils; make

collectstatic:
	cd www/js;mkdir -p bullet
	cp deps/bullet/priv/bullet.js www/js/bullet/bullet.js

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

test: clean eunit

eunit:
	@$(REBAR) eunit skip_deps=true

# ct:
# 	@$(REBAR) ct skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .talkybee_dialyzer.plt \
		--apps kernel stdlib deps/*

dialyze:
	@$(DIALYZER) --src lib/*/src --plt .talkybee_dialyzer.plt -Werror_handling \
		-Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

# start for development
start: compile
	erl -pa lib/*/ebin deps/*/ebin devutils/*/ebin \
	    -i  lib/*/include deps/*/include devutils/*/include \
	    -mnesia dir $(MNESIA_DIR) \
	    -sname $(NODE_NAME) \
	    -eval "debug:init(),\
	           debug:start(),\
		   sync:go()."

pack: init compile
	@$(REBAR) generate
	cp -r www rel/talkybee
	cd rel; tar -czvf talkybee.tar.gz talkybee
	rm -rf rel/talkybee
	mv rel/talkybee.tar.gz .
	echo "talkybee.tar.gz is created in current directory."
