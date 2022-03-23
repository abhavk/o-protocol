-module(o_node).
-export([start/1, loop/2]).

start(IDServer) -> 
  IDServer ! { self(), {get_id}},
  receive 
    X ->
      {_, {_, MyID}} = X,
      Ledger = #{}
  end,
  spawn(o_node, loop, [Ledger, MyID]).

loop(Ledger, MyID) -> 
  receive
    { Client, { send, TX }} -> 
      L2 = applyTX(TX, MyID, Ledger);
    { Client, { read }} ->
      L2 = Ledger
  end,
  Client ! { self(), { L2 } },
  loop(L2, MyID).

applyTX(TX, RewardAddr, Ledger) -> 
  Validtx = is_valid(TX, Ledger),
  if 
    Validtx -> 
        { _, From, To, Amount, Fee } = TX,
        L2 = replace(From, lookup(From, Ledger)-Amount-Fee, Ledger),
        L3 = replace(To, lookup(To, L2)+Amount, L2),
        L4 = replace(RewardAddr, lookup(RewardAddr, L3)+1+Fee, L3),
        L4;
    true -> 
        L4 = Ledger
  end,
  L4.
   

replace(Address, Amt, Ledger) ->
  Containskey = maps:is_key(Address, Ledger),
  if 
    Containskey -> 
        NewLedger = maps:update(Address, Amt, Ledger);
    true -> 
        NewLedger = maps:put(Address, Amt, Ledger)
  end,
  NewLedger.
  
lookup(Address, Ledger) -> 
  Containskey = maps:is_key(Address, Ledger),
  if 
    Containskey -> 
        Balance = maps:get(Address, Ledger);
    true ->
        Balance = 0
  end,
  Balance.

is_valid(TX, Ledger) -> 
  { _, From, _, Amount, Fee } = TX,
  Sufficient = (lookup(From, Ledger) >= Amount + Fee),
  Sufficient.