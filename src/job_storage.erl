-module(job_storage).
-behaviour(gen_server).

-export([start_link/0, store_script/1, get_last_script/0, clear/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    last_script = undefined :: undefined | binary()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_script(Script) when is_binary(Script) ->
    gen_server:cast(?MODULE, {store, Script}).

get_last_script() ->
    gen_server:call(?MODULE, get).

clear() ->
    gen_server:cast(?MODULE, clear).

init([]) ->
    {ok, #state{}}.

handle_call(get, _From, State = #state{last_script = undefined}) ->
    {reply, {error, not_found}, State};
handle_call(get, _From, State = #state{last_script = Script}) ->
    {reply, {ok, Script}, State}.

handle_cast({store, Script}, State) ->
    {noreply, State#state{last_script = Script}};

handle_cast(clear, State) ->
    {noreply, State#state{last_script = undefined}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
