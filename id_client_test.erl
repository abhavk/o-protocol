% This module is intended to test how the id_assign server works
-module(id_client).
-export([start/1]).

start(IDServer) -> 
  IDServer ! { self(), {get_id}},
  receive
    X -> 
      {_, {_, ID}} = X
  end,
  io:format("~p~n", [ID]),
  ID.