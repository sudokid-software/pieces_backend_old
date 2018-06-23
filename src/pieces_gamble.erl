-module(pieces_gamble).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2, roll/1]).

start_link() ->
    %% ServerName, Module, Args, Options
    gen_server:start_link({local, pieces_gamble}, pieces_gamble, [], []).

init(_Args) ->
    pg2:create(gamble),
    pg2:join(gamble, self()),
    {ok, []}.

%% Request, From, State
handle_call({roll}, _From, State) ->
    {reply, rand:uniform(100), State};

handle_call({roll, Number}, _From, State) when is_integer(Number) ->
    {reply, rand:uniform(Number), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({bet}, State) ->
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({roll}, State) ->
    gen_server:call(self(), {roll}),
    {noreply, State}.

terminate(_Reason, _State) ->
    {shutdown, []}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

roll(Pid) ->
    gen_server:call(Pid, {roll}).

