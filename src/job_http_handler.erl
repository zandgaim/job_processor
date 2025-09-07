-module(job_http_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([try_decode/1]).  %% for clarity, export if you also want to test it

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    logger:info("HTTP request: ~p ~p", [Method, Path]),

    case Method of
        <<"GET">> ->
            case job_storage:get_last_script() of
                {ok, Script} ->
                    Resp = cowboy_req:reply(
                        200,
                        #{<<"content-type">> => <<"text/plain">>},
                        Script,
                        Req
                    ),
                    {ok, Resp, State};
                {error, not_found} ->
                    Resp = send_response(Req, 404, #{error => <<"No script stored yet">>}),
                    {ok, Resp, State}
            end;

        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            logger:debug("Received body: ~p", [Body]),

            case try_decode(Body) of
                {ok, Request} ->
                    {ok, Resp} = process_request(Request, Req1),
                    {ok, Resp, State};
                {error, invalid_json} ->
                    Data = #{error => <<"Invalid JSON">>},
                    Resp = send_response(Req1, 400, Data),
                    {ok, Resp, State};
                {error, decode_crash} ->
                    Data = #{error => <<"Internal Server Error">>},
                    Resp = send_response(Req1, 500, Data),
                    {ok, Resp, State}
            end
    end.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

try_decode(Body) ->
    try
        {ok, jsx:decode(Body, [return_maps])}
    catch
        error:badarg ->
            {error, invalid_json};
        _ ->
            {error, decode_crash}
    end.

process_request(Request, Req1) ->
    case job_processor:process(Request) of
        {ok, Data} ->
            Resp = send_response(Req1, 200, Data),
            {ok, Resp};

        {error, bad_request} ->
            Data = #{error => <<"Bad Request">>},
            Resp = send_response(Req1, 400, Data),
            {ok, Resp};

        {error, not_found} ->
            Data = #{error => <<"Not Found">>},
            Resp = send_response(Req1, 404, Data),
            {ok, Resp};

        {error, server_error} ->
            Data = #{error => <<"Internal Server Error">>},
            Resp = send_response(Req1, 500, Data),
            {ok, Resp}
    end.

send_response(Req, Status, Data) ->
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Data),
        Req
    ).
