# Basic O-Protocol

This is a basic first implementation of the O-protocol - an infinite trustless mint. Notably, this protocol does not specify consensus requirements within itself, but leaves that for participants to decide between themselves.

At the moment it assumes that all transactions are honest/legitimate and identifies processes based on ID numbers (from a centralized ID Server). 

In order to run the `o_node`, first compile the node:
```
1> c(o_node).
{ok,o_node}
``` 

Create a node

```
2> N = o_node:start().
<0.87.0>
```

Run some `nops` transactions:

```
3> N ! { self(), { nops, 1000 }.
{<0.79.0>,{nops,1000}}

4> receive X -> X end.
{<0.87.0>,
 {<<133,19,43,157,167,245,53,38,7,30,61,140,61,18,111,85,
    44,178,164,144,154,145,253,124,101,19,...>>,
  #{<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>> =>
        0,
    <<133,19,43,157,167,245,53,38,7,30,61,140,61,18,111,85,
      44,178,164,144,154,145,253,124,...>> =>
        1000},
  "113"}}
```

The first item is the public key of the created node, the second item (beginning with `#`) is the ledger containing a mapping of public keys to number of coins owned by the address, and the final item is the number of milliseconds taken to finish this run.  

## Future updates:

- Clear separation between protocol and strategy
