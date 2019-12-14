-module(common).
-author("eschorn").
-include("constants.hrl").

-export([exp/2]).


%% Modular exponentiation via square and multiply algorithm
-spec(exp(X :: non_neg_integer(), Exp :: non_neg_integer()) -> non_neg_integer()).
exp(X, Exp) ->
  exp(X, Exp, 1).


-spec(exp(X :: non_neg_integer(), Exp :: non_neg_integer(), Result :: non_neg_integer()) -> non_neg_integer()).
exp(_X, 0, Result) ->
  Result;

exp(X, Exp, Result) ->
  if
    Exp band 1  == 1->
      R1 = (Result * X) rem ?PRIME ;
    true ->
      R1 = Result
  end,
  X1 = (X * X) rem ?PRIME,
  exp(X1, Exp bsr 1, R1).
