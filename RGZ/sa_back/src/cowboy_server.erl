-module(cowboy_server).
-export([start_link/1]).

start_link(Port) ->
    {ok, spawn_link(fun()->init(Port) end)}.

init(Port) ->
    case eredis:start_link("sa_back-db-1", 6379, 0, [], 5000) of
        {ok, Pid} ->
            register(redis, Pid),
            Dispatch = cowboy_router:compile([
                { '_', [{"/:command/[:key]", main_handler, []}] }
            ]),
            {ok, _} = cowboy:start_clear(
                cowboy_listener,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}}
            );
        Error ->
            erlang:display(Error)
    end,
    receive
    after
        6000000 -> ok
    end.