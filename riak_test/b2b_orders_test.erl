-module(b2b_orders_test).

%% API
-export([confirm/0,
  handle_event/1,
  main_test/3,
  dc1_txns/3,
  dc2_txns/3,
  dc3_txns/3,
  handle_object_invariant/2]). %%

-include_lib("eunit/include/eunit.hrl").

-define(HARNESS, (rt_config:get(rt_harness))).
-define(INITBUDGET, 270000).

-define(ITEMS, [{shirt, riak_dt_pncounter, bucket}, {pants, riak_dt_pncounter, bucket}, {dress, riak_dt_pncounter, bucket}]).
-define(UNITCOST, [{shirt, 200}, {pants, 350}, {dress, 400}]).
-define(STOREIDS, [store1]). %%[{store1, riak_dt_gcounter, bucket}]).
-define(BUDGET_UPDATE1, 20000).
-define(BUDGET_UPDATE3, 30000).

confirm() ->
  rt:setup_harness(dummy, dummy),

  NumVNodes = rt_config:get(num_vnodes, 8),
  rt:update_app_config(all, [{riak_core, [{ring_creation_size, NumVNodes}]}]),

  _Clean = rt_config:get(clean_cluster, true),
  [Cluster1, Cluster2, Cluster3] = rt:build_clusters([1,1,1]),
  rt:wait_until_ring_converged(Cluster1),
  rt:wait_until_ring_converged(Cluster2),
  rt:wait_until_ring_converged(Cluster3),

  ok = common:setup_dc_manager([Cluster1, Cluster2, Cluster3], first_run),
  io:format("~nDC setup is done for clusters.~n"),

  main_test(Cluster1, Cluster2, Cluster3).

main_test(Cluster1, Cluster2, Cluster3) ->
  Node1 = hd(Cluster1),
  Node2 = hd(Cluster2),
  Node3 = hd(Cluster3),

  %%% init budget for every store on every DC
  {ok, CTInit} = comm_test:event(?MODULE, [1, Node1, ignore, [?INITBUDGET]]),

  %%% init stock availability for every item on everey DC
%%  {ok, CTInit} = comm_test:event(?MODULE, [2, Node1, CTInit1, [2000]]),

  CT1 = dc1_txns(Node1, [], [CTInit]),
  CT2 = dc2_txns(Node2, [], [CTInit]),
  CT3 = dc3_txns(Node3, [], [CTInit]),

  %%% Specify invariant objects
  BudgetObjs = b2b_orders:create_budget_obj(all),
  comm_test:objects(?MODULE, BudgetObjs),

  Time =
    dict:merge(fun(_K, T1, T2) ->
                 max(T1, T2)
               end, CT1,
               dict:merge(fun(_K, T1, T2) ->
                            max(T1, T2)
                          end, CT2, CT3)),

  [BudgetObj] = BudgetObjs,
  Vals = [get_val(Node, BudgetObj, Time) || Node <- [Node1, Node2, Node3]],

  lager:info("Vals: ~w", [Vals]),
  lager:info("Val on node 1: ~p", [get_val(Node1, BudgetObj, Time)]),

  Quiescence_val = lists:usort(Vals),
  ?assertMatch(Quiescence_val, [hd(Vals)]),
  pass.

dc1_txns(Node, _Orders, [STInit]) ->
  io:format("~n~ntransactions on DC: ~w ", [Node]),

  Order1 = {b2b_orders:get_object_of(dress), 50, "Jan. 29"},
  Order2 = {b2b_orders:get_object_of(pants), 50, "Feb. 10"},
  Order3 = {b2b_orders:get_object_of(shirt), 50, "Feb. 15"},
  Order4 = {b2b_orders:get_object_of(shirt), 150, "March. 15"},
  Order5 = {b2b_orders:get_object_of(pants), 50, "Aug. 23"},

  {_Res1, CT1} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order1]}]]),
  {_Res2, CT2} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order2]}]]),
  {_Res3, CT3} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order3]}]]),
  {_Res4, CT4} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order4]}]]),
  {_Res5, CT5} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order5]}]]),

  vc_max(vc_max(CT1, CT2), vc_max(vc_max(CT4, CT3), CT5)).

dc2_txns(Node, _Orders, [STInit]) ->
  Order1 = {b2b_orders:get_object_of(shirt), 80, "Feb. 19"},
  {_Res1, CT1} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order1]}]]),
  Order2 = {b2b_orders:get_object_of(pants), 50, "Feb. 28"},
  {_Res2, CT2} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order2]}]]),
  Order3 = {b2b_orders:get_object_of(shirt), 150, "April. 19"},
  {_Res3, CT3} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order3]}]]),
  Order4 = {b2b_orders:get_object_of(dress), 60, "March. 29"},
  {_Res4, CT4} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order4]}]]),
  Order5 = {b2b_orders:get_object_of(pants), 50, "Jan. 28"},
  {_Res2, CT5} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order5]}]]),

  vc_max(vc_max(CT1, CT2), vc_max(vc_max(CT4, CT3), CT5)).

dc3_txns(Node, _Orders, [STInit]) ->
  {_Res, _CT} = comm_test:event(?MODULE, [1, Node, STInit, [30000]]),
  Order1 = {b2b_orders:get_object_of(dress), 50, "March. 17"},
  {_Res1, CT1} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order1]}]]),
  Order2 = {b2b_orders:get_object_of(pants), 30, "Feb. 01"},
  {_Res2, CT2} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order2]}]]),
  Order3 = {b2b_orders:get_object_of(shirt), 50, "April. 19"},
  {_Res3, CT3} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order3]}]]),
  Order4 = {b2b_orders:get_object_of(shirt), 75, "June. 21"},
  {_Res4, CT4} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order4]}]]),
  Order5 = {b2b_orders:get_object_of(pants), 40, "Sept. 11"},
  {_Res5, CT5} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order5]}]]),
  Order6 = {b2b_orders:get_object_of(pants), 30, "Feb. 01"},
  {_Res6, CT6} = comm_test:event(?MODULE, [3, Node, STInit, [{store1, [Order6]}]]),

  vc_max(vc_max(vc_max(CT1, CT2), CT3), vc_max(vc_max(CT4, CT5), CT6)).


%%%=======================================
%%% Commander Callback functions
%%%=======================================

%%% init store budgets
handle_event([1, Node, ST, AppArgs]) ->
  [Val] = AppArgs,
  {ok, CT1} = b2b_orders:init_budget(Node, ST, Val),
 {ok, CT1};

%%% init stock availability
handle_event([2, Node, ST, AppArgs]) ->
  [Count] = AppArgs,
  {ok, CT1} = b2b_orders:init_availability(Node, ST, Count),
  {ok, CT1};

handle_event([3, Node, ST, AppArgs]) ->
  [{StoreId, Orders}] = AppArgs,
  {_Res, {_TxId, CT1}} = b2b_orders:place_order(Node, ST, {StoreId, Orders}),
  {ok, CT1}.


%%%=======================================
%%% Invariant specification
%%%=======================================
handle_object_invariant(Node, [BudgetObj]) ->
  {Key, _Type, _} = BudgetObj,
  Budget =  get_val(Node, BudgetObj, ignore),
  io:format("~nBudget for Store: ~w on node: ~w became: ~p~n", [Key, Node, Budget]),
  ?assert(Budget >= 0),
  true.

%%%=======================================
%%% Internal functions
%%%=======================================
get_val(Node, Obj, Clock) ->
  {ok, Tx} = rpc:call(Node, antidote, start_transaction, [Clock, []]),
  {ok, [Res]} = rpc:call(Node, antidote, read_objects, [[Obj], Tx]),
  {ok, _CT1} = rpc:call(Node, antidote, commit_transaction, [Tx]),
  Res.

vc_max(VC1, VC2) ->
  dict:merge(fun(_K, V1, V2) ->
              if
                V1 =< V2 -> V2;
                true -> V1
              end
             end, VC1, VC2).
