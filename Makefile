REBAR = $(shell pwd)/rebar
.PHONY: rel deps test relgentlerain

all: deps compile test compile-riak-test

compile: deps
	$(REBAR) compile

compile-riak-test: compile
	$(REBAR) skip_deps=true riak_test_compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	rm -rf riak_test/ebin

cleantests:
	rm -rf riak_test/ebin

distclean: clean devclean relclean cleanplt
	$(REBAR) delete-deps

rel: all
	$(REBAR) generate

relgentlerain: export TXN_PROTOCOL=gentlerain
relgentlerain: relclean cleantests rel

relnocert: export NO_CERTIFICATION = true
relnocert: relclean cleantests rel

relclean:
	rm -rf rel/antidote

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/antidote/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/antidote/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/antidote/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/antidote/lib;)

currentdevrel: stagedevrel compile-riak-test
	riak_test/bin/antidote-current.sh

riak-test: currentdevrel
	$(foreach dep,$(wildcard riak_test/*.erl), ../riak_test/riak_test -v -c antidote -t $(dep);)

stage-riak-test: all
	$(foreach dep,$(wildcard riak_test/*.erl), ../riak_test/riak_test -v -c antidote -t $(dep);)

##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel

DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool eunit syntax_tools compiler mnesia public_key snmp

include tools.mk

typer:
	typer --annotate -I ../ --plt $(PLT) -r src


online-store-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote -t online_store_test


wallet-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{110, 220, 340}" -t wallet_test



wallet-delay0-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 0 -t wallet_test

wallet-delay1-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 1 -t wallet_test

wallet-delay2-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 2 -t wallet_test

wallet-random10-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{210, 235,280}" -t wallet_test

wallet-random11-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{330, 440, 550}" -t wallet_test

wallet-random12-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{320, 420, 520}" -t wallet_test

wallet-random13-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{1, 101, 301}" -t wallet_test

wallet-random14-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{400, 450, 500}" -t wallet_test

wallet-random15-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{260, 580, 850}" -t wallet_test

wallet-random16-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{630, 520, 410}" -t wallet_test

wallet-random17-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{999, 750, 300}" -t wallet_test

wallet-random18-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{15, 27, 38}" -t wallet_test

wallet-random19-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{1, 2, 3}" -t wallet_test

wallet-random20-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{32, 42, 52}" -t wallet_test



b2b-order-delay0-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 0 -t b2b_orders_test

b2b-order-delay1-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 1 -t b2b_orders_test

b2b-order-delay2-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 2 -t b2b_orders_test

b2b-order-random0-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{120, 460, 680}" -t b2b_orders_test

b2b-order-random1-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{330, 440, 550}" -t b2b_orders_test

b2b-order-random2-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{11, 25, 38}" -t b2b_orders_test

b2b-order-random3-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{320, 420, 520}" -t b2b_orders_test

b2b-order-random4-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{1, 101, 301}" -t b2b_orders_test

b2b-order-random5-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{400, 450, 500}" -t b2b_orders_test

b2b-order-random6-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{260, 580, 850}" -t b2b_orders_test

b2b-order-random7-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{630, 520, 410}" -t b2b_orders_test

b2b-order-random8-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{999, 750, 300}" -t b2b_orders_test

b2b-order-random9-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{15, 27, 38}" -t b2b_orders_test

b2b-order-random10-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{1, 2, 3}" -t b2b_orders_test

b2b-order-random11-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{32, 42, 52}" -t b2b_orders_test



ad-counter-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote -t ad_counter_test

ad-counter-delay0-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 0 -t ad_counter_test

ad-counter-delay1-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 1 -t ad_counter_test

ad-counter-delay2-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_delay_scheduler 2 -t ad_counter_test

ad-counter-random6-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{110, 220, 280}" -t ad_counter_test

ad-counter-random7-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{110, 220, 340}" -t ad_counter_test

ad-counter-random8-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{210, 235,280}" -t ad_counter_test

ad-counter-random9-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{11, 25, 38}" -t ad_counter_test

ad-counter-random10-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{120, 460, 680}" -t ad_counter_test

ad-counter-random11-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{330, 440, 550}" -t ad_counter_test

ad-counter-random12-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{320, 420, 520}" -t ad_counter_test

ad-counter-random13-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{1, 101, 301}" -t ad_counter_test

ad-counter-random14-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{400, 450, 500}" -t ad_counter_test

ad-counter-random15-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{260, 580, 850}" -t ad_counter_test

ad-counter-random16-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{630, 520, 410}" -t ad_counter_test

ad-counter-random17-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{999, 750, 300}" -t ad_counter_test

ad-counter-random18-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{15, 27, 38}" -t ad_counter_test

ad-counter-random19-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{1, 2, 3}" -t ad_counter_test

ad-counter-random20-test:
	$(REBAR) skip_deps=true riak_test_compile
	../riak_test/riak_test -v -c antidote comm_random_scheduler "{32, 42, 52}" -t ad_counter_test