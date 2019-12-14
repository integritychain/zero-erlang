# Zero knowledge proofs written in Erlang

This code is for strictly educational purposes, aiming for clarity through simplicity and 
brevity, not performance (or constant-time execution).

* `apps/zero/src/prover1.erl` implements an optionally dishonest state-machine proving 
   knowledge of a discrete log of a given value.
* `apps/zero/src/verifier1.erl` implements a verifier challenger against prover1
* `apps/zero/src/[common.erl|constants.erl]` provide common math and constants, respectively

The remainder of the code will automatically raise an Erlang OTP Application, a Supervisor,
and two prover1 processes: honest_prover and dishonest_prover. The can be interrogated in
a shell if desired via `verifier1:test_honest().` and `verifier1:test_dishonest().`.

Given a full Erlang and rebar3 installation, build and run by:  

> rebar3 shell
> 
> verifier1:test_dishonest().
> verifier1:test_honest().
