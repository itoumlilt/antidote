-include("antidote_message_types.hrl").

-export_type([descriptor/0, interdc_txn/0, recvr_state/0, request_cache_entry/0]).

-record(recvr_state,
        {lastRecvd :: orddict:orddict(), %TODO: this may not be required
         lastCommitted :: orddict:orddict(),
         %%Track timestamps from other DC which have been committed by this DC
         recQ :: orddict:orddict(), %% Holds receiving updates from each DC separately in causal order.
         statestore,
         partition}).
-type recvr_state() :: #recvr_state{}.

-type socket_address() :: {inet:ip_address(), inet:port_number()}.
-type zmq_socket() :: any().
-type pdcid() :: {dcid(), partition_id()}.
-type log_opid() :: non_neg_integer().

-record(interdc_txn, {
 dcid :: dcid(),
 partition :: partition_id(),
 prev_log_opid :: op_number() | none, %% the value is *none* if the transaction is read directly from the log
 snapshot :: snapshot_time(),
 timestamp :: clock_time(),
 last_update_opid :: undefined | op_number(), %% last opid of the txn that was an update operations (i.e. not a commit/abort)
 bucket :: bucket(),
 log_records :: [log_record()] %% if the OP list is empty, the message is a HEARTBEAT
}).
-type interdc_txn() :: #interdc_txn{}.

-record(descriptor, {
 dcid :: dcid(),
 partition_num :: non_neg_integer(),
 publishers :: [socket_address()],
 logreaders :: [socket_address()]
}).

-type descriptor() :: #descriptor{}.

%% This keeps information about an inter-dc request that
%% is waiting for a reply
-record(request_cache_entry, {
	  request_type :: inter_dc_message_type(),
	  func :: function() | undefined,
	  req_id_binary :: binary(),
	  pdcid :: pdcid(),
	  binary_req :: binary()
	 }).
-type request_cache_entry() :: #request_cache_entry{}.

%% This keeps information about an inter-dc request
%% on the site that is performing the query
-record(inter_dc_query_state, {
	  request_type :: inter_dc_message_type(),
	  zmq_id :: term(),
	  request_id_num_binary :: binary(),
	  local_pid :: pid()
	 }).
-type inter_dc_query_state() :: #inter_dc_query_state{}.

%% State for sub buff
-record(inter_dc_sub_buf, {
  state_name :: normal | buffering,
  pdcid :: pdcid(),
  last_observed_opid :: non_neg_integer() | init,
  queue :: queue:queue(),
  logging_enabled :: boolean(),
  log_reader_timeout :: integer()
}).
-type inter_dc_sub_buf() :: #inter_dc_sub_buf{}.
