-module(prover1).
-behaviour(gen_statem).
-author("eschorn").
-include("constants.hrl").

-export([start_link/2, get_y/1, get_c/1, get_xpr/1, get_r/1]).   %% Module API functions
-export([init/1, terminate/3, code_change/4, callback_mode/0]).  %% gen_statem callbacks
-export([state_offer_y/3, state_offer_c/3, state_offer_xr/3]).   %% Next state functions



%%%===================================================================
%%% Module API functions
%%%===================================================================

%% Called by supervisor
-spec start_link(atom(), boolean()) -> 'ignore' | {'error',_} | {'ok', pid()}.
start_link(Name, Honest) -> gen_statem:start_link({local, Name}, ?MODULE,
  [#state{honest=Honest, x=0, y=0, r=0, c=0, iter=0}], []).

%% User API below
-spec get_y(atom()) -> non_neg_integer().
get_y(Name) -> gen_statem:call(Name, get_y).

-spec get_c(atom()) -> non_neg_integer().
get_c(Name) -> gen_statem:call(Name, get_c).

-spec get_xpr(atom()) -> non_neg_integer().
get_xpr(Name) -> gen_statem:call(Name, get_xpr).

-spec get_r(atom()) -> non_neg_integer().
get_r(Name) -> gen_statem:call(Name, get_r).



%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init(list(boolean())) -> {'ok', atom(), tuple()}.
init([Honest]) ->
  _ = crypto:rand_seed(),
  {ok, state_offer_y, Honest}.

-spec callback_mode() -> 'state_functions'.
callback_mode() -> state_functions.

-spec terminate(any(), any(), any()) -> 'ok'.
terminate(_Reason, _StateName, _State) -> ok.

-spec code_change(any(), any(), tuple(), any()) -> {atom(), any(), tuple()}.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.



%%%===================================================================
%%% Internal next state functions
%%%===================================================================

%% state_offer_y: match call from get_y, calc/save X Y, reply Y, move to state_offer_c
-spec state_offer_y({atom(), any()}, atom(), tuple()) -> {atom(), atom(), tuple(), list()}.
state_offer_y({call, From}, get_y, State) ->
  case State#state.honest of
    true ->  %% Honest: calculate X and Y
      X = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME,  %% 64 bits more than needed
      Y = common:exp(?GENERATOR, X);
    false ->
      X = 0,  %% Dishonest: X is 'unknown' and not subsequently used
      Y = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME  %% Dishonest: use a random Y
  end,
  NewState = State#state{x=X, y=Y, iter=0},
  {next_state, state_offer_c, NewState, [{reply, From, Y}]};

% state_offer_y: match anything (else), reply error, move to state_offer_y
state_offer_y({_Event, From}, _Source, _State) ->
  {next_state, state_offer_y, {}, [{reply, From, error}]}.


%% state_offer_c: match call from get_c, calc/save R C, reply C, move to state_offer_xr
-spec state_offer_c({atom(), any()}, atom(), tuple()) -> {atom(), atom(), tuple(), list()}.
state_offer_c({call, From}, get_c, State) ->
  R = rand:uniform(1 bsl (1024 + 64)) rem ?PRIME,
  Rnd = rand:uniform(),  %% Needed only for dishonest prover; Predict verifier's next ask
  case (State#state.honest =:= true) or (Rnd < 0.5) of
    true -> C = common:exp(?GENERATOR, R);  %% Honest OR predict that verifier will ask R
    false ->  %% Dishonest and predict the verifier will ask for (x+r) mod (p-1)
      Gr = common:exp(?GENERATOR, R),               %% G^R
      GyInv = common:exp(State#state.y, ?PRIME-2),  %% Inv(Y)
      C = (Gr * GyInv) rem ?PRIME                   %% C = G^R * Inv(Y)
    end,
  NewState = State#state{c=C, r=R, iter=State#state.iter+1},
  {next_state, state_offer_xr, NewState, [{reply, From, C}]};

% state_offer_c: match anything (else), reply error, move to state_offer_y
state_offer_c({_Event, From}, _Source, _State) ->
  {next_state, state_offer_y, {}, [{reply, From, error}]}.


%% state_offer_xr: match call from get_xpr, calc x+r mod (p-1), reply, move to state_offer_y
-spec state_offer_xr({atom(), any()}, atom(), tuple()) -> {atom(), atom(), tuple(), list()}.
state_offer_xr({call, From}, get_xpr, State) ->
  case State#state.honest of
    true -> Xpr = (State#state.x + State#state.r) rem (?PRIME - 1); %% Honest: x+r mod (p-1)
    false -> Xpr = State#state.r  %% Dishonest: r
  end,
  {next_state, case State#state.iter < ?MAX_ITER of true -> state_offer_c;
                 false -> state_offer_y end, State, [{reply, From, Xpr}]};

%% state_offer_xr: match call from get_r, reply R, move to state_offer_y
state_offer_xr({call, From}, get_r, State) ->
  {next_state, case State#state.iter < ?MAX_ITER of true -> state_offer_c;
                 false -> state_offer_y end, State, [{reply, From, State#state.r}]};

% state_offer_xr: match anything (else), reply error, move to state_offer_y
state_offer_xr({_Event, From}, _Source, _State) ->
  {next_state, state_offer_y, {}, [{reply, From, error}]}.
