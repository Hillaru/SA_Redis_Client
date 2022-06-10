%%%-------------------------------------------------------------------
%% @doc cowboy_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => server,
                    start => {cowboy_server, start_link, [8081]},
                    type => supervisor
                   }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions