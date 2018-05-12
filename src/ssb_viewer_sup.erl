%%%-------------------------------------------------------------------
%% @doc ssb_viewer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ssb_viewer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ElliSpec = #{
      id => ssb_viewer_server,
      start => {elli, start_link, [[{callback, ssb_viewer_server}, {port, 3000}]]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [elli]
    },
    {ok, { {one_for_all, 0, 1}, [ElliSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
