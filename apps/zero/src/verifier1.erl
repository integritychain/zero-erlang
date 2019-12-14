-module(verifier1).
-author("eschorn").
-include("constants.hrl").


-export([verify/1, test_dishonest/0, test_honest/0]).


%% Count how many of 1000 dishonest provers can trick us. Depends upon ?MAX_ITER
test_dishonest() ->
  length(lists:filter(fun(_T) -> verifier1:verify(dishonest_prover) end, lists:seq(1, 1000))).

%% Count how many of 1000 honest provers are honest. Should be all of them
test_honest() ->
  length(lists:filter(fun(_T) -> verifier1:verify(honest_prover) end, lists:seq(1, 1000))).


%% Given a prover, initiate the protocol
verify(Name) ->
  Y = prover1:get_y(Name),
  verify(Name, Y, ?MAX_ITER, true).

%% Run the protocol and return the logical AND of each honesty test
verify(_Name, _Y, 0, Result) -> Result;
verify(Name, Y, Iter, Result) ->
  C = prover1:get_c(Name),
  RndF = rand:uniform(),
  Test = if
        RndF < 0.5 ->
          Cy = Y * C rem ?PRIME,
          Xpr = prover1:get_xpr(Name),
          G = common:exp(?GENERATOR, Xpr),
          Cy =:= G;
        true ->
          R = prover1:get_r(Name),
          G = common:exp(?GENERATOR, R),
          C =:= G
      end,
  verify(Name, Y, Iter - 1, Result and Test).
