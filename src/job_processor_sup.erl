-module(job_processor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", job_http_handler, []},
            {"/script", job_http_handler, []}
        ]}
    ]),
    logger:info("Starting job_processor_sup..."),
    {ok, {{one_for_one, 10, 10},
          [
              #{id => http,
                start => {cowboy, start_clear,
                          [http_listener,
                           [{port, 4020}],
                           #{env => #{dispatch => Dispatch}}]},
                restart => permanent,
                shutdown => brutal_kill,
                type => worker,
                modules => [cowboy]}
          ]}}.
