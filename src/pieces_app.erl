%%%-------------------------------------------------------------------
%% @doc pieces public API
%% @end
%%%-------------------------------------------------------------------

-module(pieces_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    pieces_twitch_irc_connect_sup:start_link(),
    %% pieces_gamble:start_link(),
    pieces_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
