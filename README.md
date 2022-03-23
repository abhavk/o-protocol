# Basic O-Protocol

This is a basic first implementation of the O-protocol - an infinite trustless mint. Notably, this protocol does not specify consensus requirements within itself, but leaves that for participants to decide between themselves.

At the moment it assumes that all transactions are honest/legitimate and identifies processes based on ID numbers (from a centralized ID Server). 

In order to run the `o_node`, first run the ID Server:
```
1> IDServer = id_assign:start().
<0.82.0>
``` 

Use this `IDServer` to create an `o_node`

```
2> Node1 = o_node:start(IDServer).
<0.84.0>
```

Send transactions to this node and view output:

```
3> Node1 ! { self(), { send, { tx, 1,1,0,0 }}}.
{<0.80.0>,{send,{tx,1,1,0,0}}}

4> receive X -> X end.
{<0.84.0>,{#{1 => 1}}}

```
