%%%-------------------------------------------------------------------
%% @doc zero top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zero_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  io:fwrite("Supervisor start_link~n",[]),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  io:fwrite("Supervisor init~n",[]),
  SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [   %% #{id => prove, start => {prover1, start_link, []}},
      #{id => honest_prover, start => {prover1, start_link, [honest_prover, true]}},
      #{id => dishonest_prover, start => {prover1, start_link, [dishonest_prover, false]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
