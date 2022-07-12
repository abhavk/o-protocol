-module(o_node).
-export([test/0]).
-export([start/0, loop/3]).

-define(DIGEST_ALG, sha256).
-define(TYPE, ed25519).
-define(SIG_ALG, eddsa).

-record(tx, {
        from,
        to,
        amount,
        fee,
        sig
}).

% external functions

start() -> 
  { Pubkey, Privkey } = new_key(),
  Ledger = #{},
  spawn(o_node, loop, [Ledger, Pubkey, Privkey]).

loop(Ledger, Pubkey, Privkey) -> 
  receive
    { Client, { nops, Count }} ->
      StartTime = erlang:monotonic_time(millisecond),
      TX = sign(Privkey, Pubkey, << 0:256 >>, 0, 0),
      { L2, Time } = applyMultipleNops(Count, TX, Pubkey, Ledger, StartTime), 
      Client ! { self(), { Pubkey, L2, Time } };
    { Client, { send, TX }} -> 
      L2 = applyTX(TX, Pubkey, Ledger, false);
    { Client, { read }} ->
      L2 = Ledger
  end,
  Client ! { self(), { Pubkey, L2 } },
  loop(L2, Pubkey, Privkey).

% internal functions
applyMultipleNops(0, _TX, _Pubkey, Ledger, StartTime) ->
	{ Ledger, integer_to_list(erlang:monotonic_time(millisecond)-StartTime) };
applyMultipleNops(Count, TX, Pubkey, Ledger, StartTime) -> 
	L2 = applyTX(TX, Pubkey, Ledger, false),
	applyMultipleNops(Count-1, TX, Pubkey, L2, StartTime).

applyTX(TX, RewardAddr, Ledger, Noverify) -> 
  if Noverify -> 
	L2 = replace(TX#tx.from, lookup(TX#tx.from, Ledger)-TX#tx.amount-TX#tx.fee, Ledger),
        L3 = replace(TX#tx.to, lookup(TX#tx.to, L2)+TX#tx.amount, L2),
        L4 = replace(RewardAddr, lookup(RewardAddr, L3)+1+TX#tx.fee, L3);
     true -> 	 
  	Validtx = is_valid(TX, Ledger),
  	if 
    		Validtx -> 
        		L2 = replace(TX#tx.from, lookup(TX#tx.from, Ledger)-TX#tx.amount-TX#tx.fee, Ledger),
        		L3 = replace(TX#tx.to, lookup(TX#tx.to, L2)+TX#tx.amount, L2),
        		L4 = replace(RewardAddr, lookup(RewardAddr, L3)+1+TX#tx.fee, L3);
    		true ->
			io:fwrite("Invalid"), 
        		L4 = Ledger
  	end
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
  Verified = verify(TX),
  Sufficient = (lookup(TX#tx.from, Ledger) >= TX#tx.amount + TX#tx.fee),
  Sufficient and Verified.

% crypto helper functions

new_key() ->
        crypto:generate_key(?SIG_ALG, ?TYPE).

tx_data(#tx { from = PubKey, to = To, amount = Amount, fee = Fee }) ->
        << PubKey/binary, To/binary, Amount:256, Fee:256 >>.

verify(TX) ->
        crypto:verify(?SIG_ALG, ?DIGEST_ALG, tx_data(TX), TX#tx.sig, [TX#tx.from, ?TYPE]).

sign(PrivKey, PubKey, To, Amount, Fee) ->
        Unsigned = #tx {
                from = PubKey,
                to = To,
                amount = Amount,
                fee = Fee
        },
        Unsigned#tx { sig = crypto:sign(?SIG_ALG, ?DIGEST_ALG, tx_data(Unsigned), [PrivKey, ?TYPE]) }.

% crypto tests
test() ->
        {PubKey, PrivKey} = new_key(),
        TX = sign(PrivKey, PubKey, << 0:256 >>, 1000, 10),
        % Happy path
        true = verify(TX),
        % Forged path
        ForgedTX = TX#tx { amount = 2000 },
        false = verify(ForgedTX),
        test_passed.
