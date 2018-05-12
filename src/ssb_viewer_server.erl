-module(ssb_viewer_server).

-export([handle/2]).
-export([handle_event/3]).

-behaviour(elli_handler).

handle(Req, Args) ->
  %% Delegate to our handler function
  io:format("Args: ~p~n", [Args]),
  io:format("Req: ~p~n", [Req]),
  handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
  {200, [], <<"Hello World!">>};

%% Return 404 to any other requests
handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.

handle_event(Event, _Data, _Args) ->
  io:format("Event: ~p~n", [Event]),
  ok.
