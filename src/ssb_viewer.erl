-module(ssb_viewer).

-export ([main/1]).

main(Args) ->
  X = io:get_chars(stdin, 64),
  io:put_chars(stderr, "123")
  % io:format("~p~n", Args),
  % io:format("~p~n", X),
  erlang:halt(0).
