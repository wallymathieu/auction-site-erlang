-module(auction_site_supersup).
-behaviour(supervisor).
-export([start_link/0, start_pool/3, stop_pool/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, auction_site}, ?MODULE, []).

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                 {auction_site_sup, start_link, [Name, Limit, MFA]},
                  permanent, 10500, supervisor, [auction_site_sup]},
    supervisor:start_child(auction_site, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(auction_site, Name),
    supervisor:delete_child(auction_site, Name).

init([]) ->
    MaxRestart = 6,
    MaxTime = 3000,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.
