% This is a temporary ID assigner for each process in the O network
% This should ideally be replaced by the crypto module - then the IDs will just be the public keys in the crypto module
-module(id_assign).
-export([start/0, serveNext/2]).

start() -> 
  ProcToIDMap = #{},
  spawn(id_assign, serveNext, [ProcToIDMap, 0]).

serveNext(Map, CurrentID) -> 
  receive
    { Client, {get_id} } -> 
      { ID, NextID, NewMap} = getID(Client, CurrentID, Map),
      Client ! { self(), {id, ID} }
  end,
  serveNext(NewMap, NextID).

getID(Proc, CurrentID, Map) ->
  Containskey = maps:is_key(Proc, Map),
  if 
    Containskey -> 
        NextID = CurrentID,
        M2 = Map,
        ID = maps:get(Proc, Map);
    true -> 
        NextID = CurrentID + 1,
        M2 = maps:put(Proc, NextID, Map),
        ID = NextID
  end,
  { ID, NextID, M2}.