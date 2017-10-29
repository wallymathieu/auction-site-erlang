-module(auction_site_tests).
-include_lib("eunit/include/eunit.hrl").
-export([test_mfa/1, wait_mfa/1]).

%%% All Test Fixtures
start_test_() ->
    {"It should be possible to start a pool server and give it a name",
     {setup,
      fun find_unique_name/0,
      fun(Name) ->
        [start_and_test_name(Name)]
      end}}.

mfa_test_() ->
    {"A pool process can be allocated which will be ordered "
     "to run an MFA call determined at start time, with arguments "
     "provided at call time",
     {setup,
      fun start_auction_site/0,
      fun kill_auction_site/1,
      fun(Name) ->
        [pool_run_mfa(Name)]
      end}
    }.

alloc_test_() ->
    {"A pool process can be allocated which will be ordered "
     "to run a worker, only if there are enough which "
     "haven't been ordered to run yet.",
     {setup,
      fun start_auction_site/0,
      fun kill_auction_site/1,
      fun(Name) ->
        [pool_run_alloc(Name),
         pool_run_noalloc(Name)]
      end}
    }.

realloc_test_() ->
    {"When an allocated process dies, "
     "A new one can be allocated to replace it.",
     {setup,
      fun start_auction_site/0,
      fun kill_auction_site/1,
      fun(Name) ->
        [pool_run_realloc(Name)]
      end}
    }.

queue_test_() ->
    {"The queue function can be used to run the function as soon as possible. "
     "If no space is available, the worker call is added to the queue.",
     {foreach,
      fun start_auction_site/0,
      fun kill_auction_site/1,
      [fun(Name) -> test_async_queue(Name) end,
       fun(Name) -> test_sync_queue(Name) end]}
    }.

supervision_test_() ->
    {"The auction_site will never restart a dead child, but all children (OTP "
     "compliant) will be shut down when closing the pool, even if they "
     "are trapping exits",
     {setup,
      fun find_unique_name/0,
      fun test_supervision/1}}.
    
auth_test_() ->
    {"The auction_site should only dequeue tasks after receiving a down signal "
     "from a worker and nobody else",
     {setup,
      fun start_auction_site/0,
      fun kill_auction_site/1,
      fun test_auth_dealloc/1}}.

%%% Setups/teardowns
find_unique_name() ->
    application:start(auction_site),
    Name = list_to_atom(lists:flatten(io_lib:format("~p",[now()]))),
    ?assertEqual(undefined, whereis(Name)),
    Name.

start_auction_site() ->
    Name = find_unique_name(),
    auction_site:start_pool(Name, 2, {auction_site_nagger, start_link, []}),
    Name.

kill_auction_site(Name) ->
    auction_site:stop_pool(Name).
    
%%% Actual tests
start_and_test_name(Name) ->
    auction_site:start_pool(Name, 1, {auction_site_nagger, start_link, []}),
    A = whereis(Name),
    auction_site:stop_pool(Name),
    timer:sleep(100),
    B = whereis(Name),
    [?_assert(undefined =/= A),
     ?_assertEqual(undefined, B)].

pool_run_mfa(Name) ->
    auction_site:run(Name, [i_am_running, 1, 1, self()]),
    X = receive
        {_Pid, i_am_running} -> ok
    after 3000 ->
        timeout
    end,
    ?_assertEqual(ok, X).

pool_run_alloc(Name) ->
    {ok, Pid} = auction_site:run(Name, [i_am_running, 1, 1, self()]),
    X = receive
        {Pid, i_am_running} -> ok
    after 3000 ->
        timeout
    end,
    [?_assert(is_pid(Pid)),
     ?_assertEqual(ok, X)].

pool_run_noalloc(Name) ->
    %% Init function should have set the limit to 2
    auction_site:run(Name, [i_am_running, 300, 1, self()]),
    auction_site:run(Name, [i_am_running, 300, 1, self()]),
    X = auction_site:run(Name, [i_am_running, 1, 1, self()]),
    ?_assertEqual(noalloc, X).
    
pool_run_realloc(Name) ->
    %% Init function should have set the limit to 2
    {ok, A} = auction_site:run(Name, [i_am_running, 500, 1, self()]),
    timer:sleep(100),
    {ok, B} = auction_site:run(Name, [i_am_running, 500, 1, self()]),
    timer:sleep(600),
    {ok, Pid} = auction_site:run(Name, [i_am_running, 1, 1, self()]),
    timer:sleep(100),
    L = flush(),
    [?_assert(is_pid(Pid)),
     ?_assertEqual([{A,i_am_running}, {B,i_am_running}, {Pid,i_am_running}],
                   L)].

test_async_queue(Name) ->
    %% Still two elements max!
    ok = auction_site:async_queue(Name, [i_am_running, 2000, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 2000, 1, self()]),
    noalloc = auction_site:run(Name, [i_am_running, 2000, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 500, 1, self()]),
    timer:sleep(3500),
    L = flush(),
    ?_assertMatch([{_, i_am_running}, {_, i_am_running}, {_, i_am_running}], L).

test_sync_queue(Name) ->
    %% Hell yase, two max
    {ok, Pid} = auction_site:sync_queue(Name, [i_am_running, 200, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 200, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 200, 1, self()]),
    {ok, Pid2} = auction_site:sync_queue(Name, [i_am_running, 100, 1, self()]),
    timer:sleep(300),
    L = flush(),
    [?_assert(is_pid(Pid)),
     ?_assert(is_pid(Pid2)),
     ?_assertMatch([{_,i_am_running}, {_,i_am_running},
                    {_,i_am_running}, {_,i_am_running}],
                   L)].

test_supervision(Name) ->
    auction_site:start_pool(Name, 1, {auction_site_nagger, start_link, []}),
    {ok, Pid} = auction_site:run(Name, [sup, 10000, 100, self()]),
    auction_site:stop_pool(Name),
    timer:sleep(100),
    ?_assertEqual(undefined, process_info(Pid)). 

test_auth_dealloc(Name) ->
    %% Hell yase, two max
    {ok, _Pid} = auction_site:sync_queue(Name, [i_am_running, 500, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 10000, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 10000, 1, self()]),
    ok = auction_site:async_queue(Name, [i_am_running, 1, 1, self()]),
    timer:sleep(600),
    Name ! {'DOWN', make_ref(), process, self(), normal},
    Name ! {'DOWN', make_ref(), process, self(), normal},
    Name ! {'DOWN', make_ref(), process, self(), normal},
    timer:sleep(200),
    L = flush(),
    ?_assertMatch([{_,i_am_running}], L).
    


flush() ->
    receive
        X -> [X|flush()]
    after 0 ->
        []
    end.

%% Exported Helper functions
test_mfa(Pid) ->
    Pid ! i_am_running.

wait_mfa(Pid) ->
    Pid ! i_am_running,
    timer:sleep(3000).
