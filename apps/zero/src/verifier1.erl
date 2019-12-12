-module(verifier1).
-author("eschorn").
-include("defs.hrl").


%% API
-export([verify/1, test_dishonest/0, test_honest/0]).

test_dishonest() ->
  length(lists:filter(FunD = fun(T) -> verifier1:verify(dishonest_prover) end, lists:seq(1, 10000))).

test_honest() ->
  length(lists:filter(FunH = fun(T) -> verifier1:verify(honest_prover) end, lists:seq(1, 10000))).

verify(Name) ->
  Y = prover1:get_y(Name),
  verify(Name, Y, ?MAX_ITER, true).


verify(_Name, _Y, 0, Result) -> Result;
verify(Name, Y, Iter, Result) ->
  C = prover1:get_c(Name),
  RndF = rand:uniform(),
  Test = if
        RndF < 0.5 ->
          Cy = Y * C rem ?PRIME,
          Xpr = prover1:get_xpr(Name),
          G = prover1:exp(?GENERATOR, Xpr),
          Cy =:= G;
        true ->
          R = prover1:get_r(Name),
          G = prover1:exp(?GENERATOR, R),
          C =:= G
      end,
  verify(Name, Y, Iter - 1, Result and Test).
