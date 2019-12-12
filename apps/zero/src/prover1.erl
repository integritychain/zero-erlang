-module(prover1).
-behaviour(gen_statem).
-author("eschorn").
-include("defs.hrl").

-export([start_link/2, get_y/1, get_c/1, get_xpr/1, get_r/1, exp/2]).  %% Module API functions
-export([init/1, terminate/3, code_change/4, callback_mode/0]).        %% gen_statem callbacks
-export([state_offer_y/3, state_offer_c/3, state_offer_xr/3]).         %% Next state functions


%% TODO:
%%  1. Tighten up  -spec
%%  2. Revist stats

%%%===================================================================
%%% Module API functions
%%%===================================================================

-spec start_link(any(), any()) -> any().
start_link(Name, Honest) -> gen_statem:start_link({local, Name}, ?MODULE, [#state{honest=Honest}], []).

-spec get_y(any()) -> any().
get_y(Name) -> gen_statem:call(Name, get_y).

-spec get_c(any()) -> any().
get_c(Name) -> gen_statem:call(Name, get_c).

-spec get_xpr(any()) -> any().
get_xpr(Name) -> gen_statem:call(Name, get_xpr).

-spec get_r(any()) -> any().
get_r(Name) -> gen_statem:call(Name, get_r).



%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init(list()) -> {'ok',_,_}.
init([Honest]) ->
  _ = crypto:rand_seed(),
  {ok, state_offer_y, Honest}.

-spec callback_mode() -> 'handle_event_function' | 'state_functions'.
callback_mode() -> state_functions.

-spec terminate(any(), any(), any()) -> atom().
terminate(_Reason, _StateName, _State) -> ok.

-spec code_change(any(), any(), any(), any()) -> tuple().
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.



%%%===================================================================
%%% Internal next state functions
%%%===================================================================

%% In state_offer_y: match call from get_y, calc/save X Y, reply Y, and move to state_offer_c
-spec state_offer_y(tuple(), atom(), any()) -> tuple().
state_offer_y({call, From}, get_y, State) ->
  case State#state.honest of
    true ->   %% Honestly calculate X and Y
      X = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME,  %% 64 bits more than needed
      Y = exp(?GENERATOR, X);
    false ->  %% Dishonest: random rubbish for now, and try to cover up later
      X = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME,
      Y = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME
  end,
  NewState = State#state{x=X, y=Y, iter=0},
  {next_state, state_offer_c, NewState, [{reply, From, Y}]};

% In state_offer_y: match anything (else), reply error, and move to state_offer_y
state_offer_y({_Event, From}, _Source, _State) ->
  {next_state, state_offer_y, [], [{reply, From, error}]}.


%% In state_offer_c: match call from get_c, calc/save R C, reply C, and move to state_offer_xr
-spec state_offer_c(tuple(), atom(), any()) -> tuple().
state_offer_c({call, From}, get_c, State) ->
  R = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME,
  Rnd = rand:uniform(),  %% Needed only for dishonest prover; Predict the verifier's next ask
  case (State#state.honest =:= true) or (Rnd < 0.5) of
    true -> C = exp(?GENERATOR, R);  %% Honest or predict that verifier will ask for R
    false ->                         %% Dishonest and predict the verifier will ask for (x+r) mod (p-1)
      Gr = exp(?GENERATOR, R),              %% G^R
      Gx = exp(?GENERATOR, State#state.x),  %% G^X
      GxInv = exp(Gx, ?PRIME-2),            %% Inv(G^X)
      C = (Gr * GxInv) rem ?PRIME           %% G^R * Inv(G^X)
    end,
  NewState = State#state{c=C, r=R, iter=State#state.iter+1},
  {next_state, state_offer_xr, NewState, [{reply, From, C}]};

% In state_offer_c: match anything (else), reply error, and move to state_offer_y
state_offer_c({_Event, From}, _Source, _State) ->
  {next_state, state_offer_y, [], [{reply, From, error}]}.


%% In state_offer_xr: match call from get_xpr, calc x+r mod (p-1), reply result, and move to state_offer_y
-spec state_offer_xr(tuple(), atom(), any()) -> tuple().
state_offer_xr({call, From}, get_xpr, State) ->
  Xpr = (State#state.x + State#state.r) rem (?PRIME - 1),
  {next_state, case State#state.iter < ?MAX_ITER of true -> state_offer_c; false -> state_offer_y end,
    State, [{reply, From, Xpr}]};

%% In state_offer_xr: match call from get_r, reply R, and move to state_offer_y
state_offer_xr({call, From}, get_r, State) ->
  {next_state, case State#state.iter < ?MAX_ITER of true -> state_offer_c; false -> state_offer_y end,
    State, [{reply, From, State#state.r}]};

% In state_offer_xr: match anything (else), reply error, and move to state_offer_y
state_offer_xr({_Event, From}, _Source, _State) ->
  {next_state, state_offer_y, [], [{reply, From, error}]}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Modular exponentiation via square and multiply algorithm
-spec(exp(X :: non_neg_integer(), Exp :: non_neg_integer()) -> non_neg_integer()).
exp(X, Exp) ->
  exp(X, Exp, 1).

-spec(exp(X :: non_neg_integer(), Exp :: non_neg_integer(), Result :: non_neg_integer())
      -> non_neg_integer()).
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
