-module(b2b_orders).
%%% This module represents the backend system for a
%%% manufacturer, that receives and processes orders for a shop
%%% from clients.

%% API
-export([init_budget/3 ,
  place_order/3,
  init_availability/3,
  create_budget_obj/1,
  get_object_of/1]).

-define(ITEMS, [{shirt, riak_dt_pncounter, bucket}, {pants, riak_dt_pncounter, bucket}, {dress, riak_dt_pncounter, bucket}]).
-define(UNITCOST, [{shirt, 200}, {pants, 350}, {dress, 400}]).
-define(STOREIDS, [store1]). %%[{store1, riak_dt_gcounter, bucket}]).

init_budget(Node, Clock, Value) ->
  {ok, TxId} = rpc:call(Node, antidote, start_transaction, [Clock, []]),
  lists:foreach(fun(StoreId) ->
                  StoreBudgetObj = create_budget_obj(StoreId),
                  ok = rpc:call(Node, antidote, update_objects, [[{StoreBudgetObj, increment, Value}], TxId])
                end, ?STOREIDS),
  {ok, CT} = rpc:call(Node, antidote, commit_transaction, [TxId]),
  {ok, CT}.

init_availability(Node, Clock, Value) ->
  {ok, TxId} = rpc:call(Node, antidote, start_transaction, [Clock, []]),
  lists:foreach(fun(Item) ->
                  ok = rpc:call(Node, antidote, update_objects, [[{Item, increment, Value}], TxId])
                end, ?ITEMS),
  {ok, CT} = rpc:call(Node, antidote, commit_transaction, [TxId]),
  {ok, CT}.

place_order(Node, Clock, {StoreId, Orders}) ->
  StoreOrdersObj = create_orders_obj(StoreId),
  StoreBudgetObj = create_budget_obj(StoreId),
  {ok, TxId} = rpc:call(Node, antidote, start_transaction, [Clock, []]),
  {ok, [CurrBudget]} = rpc:call(Node, antidote, read_objects, [[StoreBudgetObj], TxId]),
  {ok, [_OrdersHistory]} = rpc:call(Node, antidote, read_objects, [[StoreOrdersObj], TxId]),

  io:format("~n**********In app, BudgetObj: ~w~nCurrBudget: ~w~n", [StoreBudgetObj, CurrBudget]),

  {OrderTotalCost, InStock} =
    lists:foldl(fun(Order, {Total, _Available}) ->
                  {Item, OrderedCount, _} = Order,
%%                  Available1 =
%%                    case Available of
%%                      true ->
%%                        {ok, [AvailableCount]} = rpc:call(Node, antidote, read_objects, [[Item], TxId]), %%stock_availability(Item),
%%                        io:format("~n****Available Count: ~w~n", [AvailableCount]),
%%                        if
%%                          OrderedCount =< AvailableCount -> true;
%%                          true -> false
%%                        end;
%%                      false ->
%%                        false
%%                    end,
%%                  io:format("~n******NewAvailable: ~w~n", [Available1]),
                  {ItemId, _, _} = Item,
                  CPU = proplists:get_value(ItemId, ?UNITCOST),
                  io:format("~n******CPU: ~w~n", [CPU]),
                  {Total + (CPU * OrderedCount), true} %%Available1
                 end, {0, true}, Orders),
  io:format("~n******TotalCost: ~w~n********InStock: ~w~n", [OrderTotalCost, InStock]),
  Res =
    if
      InStock andalso OrderTotalCost =< CurrBudget ->
        io:format("~n******Purchace: true~n"),
        ok = rpc:call(Node, antidote, update_objects, [[{StoreBudgetObj, decrement, OrderTotalCost}], TxId]),
%%        lists:foreach(fun(Order) ->
%%                        {Item, OrderedCount, _} = Order,
%%                        ok = rpc:call(Node, antidote, update_objects, [[Item, decrement, OrderedCount], TxId]),
%%                        ok = rpc:call(Node, antidote, update_objects, [[StoreOrdersObj, add, Order], TxId])
%%                      end, Orders),
        ok;
      true -> %%rejected
        ok = rpc:call(Node, antidote, update_objects,
          [[{StoreBudgetObj, decrement, 1}, {StoreBudgetObj, increment, 1}], TxId])
    end,
  {ok, CT} = rpc:call(Node, antidote, commit_transaction, [TxId]),
  case Res of
    ok -> {ok, {TxId, CT}};
    rejected -> {rejected, {notxn, Clock}}
  end.

%%%=======================================
%%% Internal functions
%%%=======================================

create_orders_obj(StoreId) ->
  {list_to_atom(atom_to_list(StoreId) ++ "_orders"), riak_dt_gset, bucket}.

create_budget_obj(all) ->
  [create_budget_obj(StoreId) || StoreId <- ?STOREIDS];

create_budget_obj(StoreId) ->
  {list_to_atom(atom_to_list(StoreId) ++ "_budget"), riak_dt_pncounter, bucket}.

get_object_of(ItemType) ->
  lists:keyfind(ItemType, 1, ?ITEMS).


%%% returns the list of items and the number
%%% of them available in the stock
%%stock_availability() ->

%%% returns the number of Items available in stock
%%stock_availability(Node, ST, Item) ->
%%  {ok, Tx} = rpc:call(Node, antidote, start_transaction, [ST, []]),
%%  {ok, [Count]} = rpc:call(Node, antidote, read_objects, [[Item], Tx]),
%%  Count.