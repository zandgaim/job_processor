-module(job_processor).
-export([process/1]).

process(#{<<"tasks">> := []}) ->
    {error, bad_request};
process(#{<<"tasks">> := Tasks}) ->
    logger:info("Processing ~p tasks", [length(Tasks)]),
    case topo_sort(Tasks) of
        {ok, Sorted} ->
            logger:info("Successfully sorted ~p tasks", [length(Sorted)]),
            generate_and_store_script(Sorted),
            {ok, #{<<"tasks">> => Sorted}};
        {error, _} = Error ->
            Error
    end;
process(_) ->
    {error, bad_request}.

generate_and_store_script(Tasks) when Tasks =:= [] ->
    {error, bad_request};
generate_and_store_script(Tasks) ->
    Commands = [maps:get(<<"command">>, T) || T <- Tasks],
    Bin =
        lists:foldl(
            fun(Cmd, Acc) ->
                <<Acc/binary, Cmd/binary, "\n">>
            end,
            <<"#!/usr/bin/env bash\n">>,
            Commands
        ),
    logger:info("Generated bash script with ~p commands", [length(Commands)]),
    job_storage:store_script(Bin),
    {ok, Bin}.

%% Topological sort
topo_sort(Tasks) ->
    Map = build_map(Tasks),
    Names = maps:keys(Map),
    topo_sort(Names, Map).

build_map(Tasks) ->
    lists:foldl(
        fun(T, Acc) ->
            N = maps:get(<<"name">>, T),
            Deps = maps:get(<<"requires">>, T, []),
            maps:put(N, T#{<<"requires">> => Deps}, Acc)
        end,
        #{},
        Tasks
    ).

topo_sort(Names, Map) ->
    topo_sort(Names, Map, [], []).
topo_sort([], Map, Order, _) -> 
    Result = [
        maps:remove(<<"requires">>, maps:get(N, Map))
        || N <- lists:reverse(Order)
    ],
    {ok, Result};
topo_sort(Names, Map, Order, Seen) ->
    {Ready, Waiting} =
        lists:partition(
          fun(N) ->
              Deps = maps:get(<<"requires">>, maps:get(N, Map), []),
              lists:all(fun(D) -> lists:member(D, Seen) end, Deps)
          end,
          Names),
    case Ready of
        [] -> 
            logger:error("Cycle detected in task dependencies: ~p", [Waiting]),
            {error, server_error};
        _ ->
            topo_sort(Waiting, Map, Ready ++ Order, Ready ++ Seen)
    end.