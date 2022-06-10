-module(main_handler).
-behaviour(cowboy_handler).
-export([init/2]).

debug_out(Req, Message) ->
    erlang:display(<<"----------------new request------------------">>),
    erlang:display(cowboy_req:method(Req)),
    erlang:display(cowboy_req:path(Req)),
    erlang:display(cowboy_req:qs(Req)),
    erlang:display(Message).

handle_get(Req0, Command, Key) ->
    RespContentType = <<"application/json">>,
    Req = cowboy_req:set_resp_header(<<"content-type">>, RespContentType, Req0),
    RespRaw =
        case Command of
            <<"mget">> ->
                DataDirty = cowboy_req:parse_qs(Req),
                Data = [K || {K, _V} <- DataDirty],
                Resp = eredis:q(redis, ["MGET" | Data]),
                Resp;
            <<"hgetall">> ->
                Resp = eredis:q(redis, ["HGETALL", Key]),
                Resp;
            <<"lrange">> ->
                Resp = eredis:q(redis, ["LRANGE", Key, 0, -1]),
                Resp;
            _ ->
                "Error. Unknown command '" ++ Command ++ "'"
        end,
    debug_out(Req, RespRaw),
    case RespRaw of
        {ok, Content} ->
            ReqFinal = cowboy_req:set_resp_header(<<"status">>, 200, Req),
            RespFinal = jsone:encode({Command, Key, Content}),
            cowboy_req:set_resp_body(RespFinal, ReqFinal);
        _ ->
            cowboy_req:set_resp_header(<<"status">>, 404, Req)
    end.

handle_post(Req, Command, Key) ->
    {ReqReaden, RespRaw} =
        case Command of
            <<"mset">> ->
                {ok, Data, Req1} = cowboy_req:read_urlencoded_body(Req),
                Body = lists:append([[K, V] || {K, V} <- Data]),   
                Resp = eredis:q(redis, ["MSET" | Body]),
                {Req1, Resp};
            <<"hmset">> ->
                {ok, Data, Req1} = cowboy_req:read_urlencoded_body(Req),
                Body = lists:append([[K, V] || {K, V} <- Data]),   
                Resp = eredis:q(redis, ["HMSET", Key | Body]),
                {Req1, Resp};
            <<"lpush">> ->
                {ok, [{Val, _} | _], Req1} = cowboy_req:read_urlencoded_body(Req),
                Resp = eredis:q(redis, ["LPUSH", Key, Val]),
                {Req1, Resp};
            <<"rpush">> ->
                {ok, [{Val, _} | _], Req1} = cowboy_req:read_urlencoded_body(Req),
                Resp = eredis:q(redis, ["RPUSH", Key, Val]),
                {Req1, Resp};
            _ ->
                "Error. Unknown command '" ++ Command ++ "'"
        end,
    debug_out(ReqReaden, RespRaw),
    case RespRaw of
        {ok, Content} ->
            ReqFinal = cowboy_req:set_resp_header(<<"status">>, 201, ReqReaden),
            RespFinal = jsone:encode({Command, Key, Content}),
            cowboy_req:set_resp_body(RespFinal, ReqFinal);
        _ ->
            cowboy_req:set_resp_header(<<"status">>, 404, ReqReaden)
    end.

handle_delete(Req, Command, Key) ->
    RespRaw =
        case Command of
            <<"rpop">> ->
                eredis:q(redis, ["RPOP", Key]);
            <<"lpop">> ->
                eredis:q(redis, ["LPOP", Key]);
            _ ->
                "Error. Unknown command '" ++ Command ++ "'"
        end,
    debug_out(Req, RespRaw),
    case RespRaw of
        {ok, _} ->
            cowboy_req:set_resp_header(<<"status">>, 204, Req);
        _ ->
            cowboy_req:set_resp_header(<<"status">>, 404, Req)
    end.

init(Req, State) ->
    Command = cowboy_req:binding(command, Req),
    Key = cowboy_req:binding(key, Req, none),

    case cowboy_req:path(Req) of
        <<"/favicon.ico">> ->
            ok,
            {ok, Req, State};
        _ ->
            Req1 =
                case cowboy_req:method(Req) of
                    <<"GET">> ->
                        handle_get(Req, Command, Key);
                    <<"POST">> ->
                        handle_post(Req, Command, Key);
                    <<"DELETE">> ->
                        handle_delete(Req, Command, Key);
                    _ ->
                        cowboy_req:set_resp_header(<<"status">>, 405, Req)
                end,
            %erlang:display(Req1),
            Req2 = cowboy_req:reply(cowboy_req:resp_header(<<"status">>, Req1), Req1),
            erlang:display(<<"--------------response send------------------">>),
            {ok, Req2, State}
    end.
