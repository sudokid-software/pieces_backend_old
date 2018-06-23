%%%-------------------------------------------------------------------
%% @doc pieces top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pieces_twitch_irc_connect_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 1, 60},
          [
           {irc, {pieces_twitch_irc_connect, start_link, []}, permanent, brutal_kill, worker, [pieces_twitch_irc_connect]}
          ]
         }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
