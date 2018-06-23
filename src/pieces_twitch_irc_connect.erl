-module(pieces_twitch_irc_connect).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).
-export([send_msg/2]).

%% -record(state, {socket, json=#{roll => #{asdf => a}, b => 3, c=> 4, "a" => 1, "b" => 2, "c" => 4}}).

start_link() ->
    %% ServerName, Module, Args, Options
    gen_server:start_link({local, pieces_twitch_irc_connect}, pieces_twitch_irc_connect, [], []).

start_link([]) ->
    %% ServerName, Module, Args, Options
    gen_server:start_link({local, pieces_twitch_irc_connect}, pieces_twitch_irc_connect, [], []).

init(_Args) ->
    %% pieces_twitch_irc_connect_sup:start_link(),
    {ok, Socket} = ssl:connect("irc.chat.twitch.tv", 443, []),
    ok = ssl:send(Socket, "PASS " ++ os:getenv("TWITCH_OAUTH") ++ "\r\n"),
    ok = ssl:send(Socket, "NICK sudokid\r\n"),
    ok = ssl:send(Socket, "JOIN #sudokid\r\n"),
    ok = ssl:send(Socket, "CAP REQ :twitch.tv/membership\r\n"),
    ok = ssl:send(Socket, "CAP REQ :twitch.tv/commands\r\n"),
    ok = ssl:send(Socket, "CAP REQ :twitch.tv/tags\r\n"),
    {ok, Socket}.

%% Request, From, State
handle_call(Request, From, Socket) ->
    io:format("HandleCall: Request: ~ts", [Request]),
    {reply, From, Socket}.

%% Request, State
handle_cast(Request, Socket) ->
    io:format("HandleCast: Request: ~ts", [Request]),
    {noreply, Socket}.

%% Ping/Pong
handle_info({ssl, _, Msg}, Socket) when Msg == "PING :tmi.twitch.tv\r\n" ->
    %% io:format("HandleInfo: Msg: ~ts", [Msg]),
    ssl:send(Socket, "PONG :tmi.twitch.tv\r\n"),
    {noreply, Socket};

%% Peoples messages
handle_info({ssl, _, IncomingMsg}, Socket) ->
    io:format("~ts~n", [IncomingMsg]),
    [UserInfo|Msg] = string:split(string:trim(IncomingMsg, trailing), ".tmi.twitch.tv PRIVMSG #sudokid :"),
    User = parse_user_info(UserInfo),
    %% io:format("~ts: ~ts~n", [User, Msg]),
    %% GamblePid = pg2:get_colsest_pid(gamble),
    command({string:slice(Msg, 0, 1), User, string:slice(Msg, 1)}, Socket),
    {noreply, Socket};

handle_info(Msg, State) ->
    io:format("~ts", [Msg]),
    {noreply, State}.

terminate(_Reason, Socket) ->
    ok = ssl:send(Socket, "PART #sudokid").

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_msg(Msg, Socket) ->
    ok = ssl:send(Socket, "PRIVMSG #sudokid :" ++ Msg ++ "\r\n").

parse_user_info(UserInfo) ->
    [_|UserString] = string:split(UserInfo, "!"),
    [User|_] = string:split(UserString, "@"),
    User.

random_choice(Responses) ->
    lists:nth(rand:uniform(length(Responses)), Responses).

%% command({FirstChar, User, RawMsg}, Socket) when FirstChar == "!" ->
command({"!", User, RawMsg}, Socket) ->
    [Command|Msg] = string:split(string:to_lower(string:trim(RawMsg, trailing, "\r\n")), " "),
    command({string:find(Msg, "/"), Command, Msg, User}, Socket);

command({nomatch, Command, Msg, User}, Socket) ->
    command({Command, Msg, User}, Socket);

command({"crashcode", _Msg, _User}, Socket) ->
    Responses = [
        "You can not crash me!",
        "You man not crash me!"
    ],
    send_msg(random_choice(Responses), Socket);

command({"project", _Msg, _User}, Socket) ->
    send_msg("We are working on a bot system called Pieces!", Socket);

command({"discord", _Msg, _User}, Socket) ->
    send_msg("https://discord.gg/dazGvKQ", Socket);

command({"help", _Msg, _User}, Socket) ->
    Responses = [ "You may type any of the following commands!", "!help - Will print this help message back.",
        "!discord - Will provide a link to the discord server.",
        "!project - Will provide back the project details."
    ],
    lists:map(fun(X) -> send_msg(X, Socket) end, Responses);

command({"commands", Msg, User}, Socket) ->
    command({"help", Msg, User}, Socket);

command({"command", Msg, User}, Socket) ->
    command({"help", Msg, User}, Socket);

command({"emos", _Msg, _User}, Socket) ->
    Responses = [
        "I hate my parents and pain is my life!",
        "I cut myself to feel!",
        "Pain is my life!",
        "Existence is pain"
    ],
    send_msg(random_choice(Responses), Socket);

command({"sjw", _Msg, User}, Socket) ->
    Responses = [
        "@" ++ User ++ " is TRIGGERED!",
        "DID YOU JUST ASSUME MY GENDER?!"
    ],
    send_msg(random_choice(Responses), Socket);

command({"thanos_snap", _Msg, _User}, Socket) ->
    Responses = [
        "You where spared by Thanos.",
        "You where slain by Thanos, for the good of the Universe."
    ],
    send_msg(random_choice(Responses), Socket);

command({"thanos", _Msg, _User}, Socket) ->
    Responses = [
        "Chick came up to me today and asked for my autograph...then said that she loved barney as a kid..bitch I ain't no barney. Punched her with the gauntlet real quick..",
        "Call me thanos the magician, I make half the bitches disappear.",
        "Bitch, I'll hit you with this planet.",
        "I poop purple just fyi",
        "Hey Vision, where you at cuz? BibleThump BibleThump "
    ],
    send_msg(random_choice(Responses), Socket);

command({"toss_kids", _Msg, User}, Socket) ->
    Response = "@" ++ User ++ " you shouldn't toss Sudos! #childabuse",
    send_msg(Response, Socket);

command({"punctuation", _Msg, _User}, Socket) ->
    Responses = [
        "Let's eat, Grandma. Let's eat Grandma."
    ],
    send_msg(random_choice(Responses), Socket);

command({"puncutation", _Msg, _User}, Socket) ->
    Responses = [
        "Let's eat, Grandma. Let's eat Grandma."
    ],
    send_msg(random_choice(Responses), Socket);

command({"typeo", Msg, User}, Socket) ->
    command({"typo", Msg, User}, Socket);

command({"typo", Msg, _User}, Socket) ->
    [Name|_] = string:split(Msg, " "),
    send_msg(Name ++ " You speld something wrong!", Socket);

command({"grammar_nazi", Msg, User}, Socket) ->
    [Name|_] = string:split(Msg, " "),
    Response = "@" ++ User ++ " thinks " ++ Name ++ " is a grammer nazi!",
    send_msg(Response, Socket);

command({"leet_speek?", _Msg, User}, Socket) ->
    Response = "@" ++ User ++ " its obfiscating your input with misspellings and numbers/symbols that make sense to human vision",
    send_msg(Response, Socket),
    send_msg("Example: 8085 is Bobs!", Socket);

command({"boobs", _Msg, User}, Socket) ->
    Responses = [
        " ( o Y o )",
        " ( . )( . )",
        " ( O Y O )"
    ],
    send_msg("@" ++ User ++ random_choice(Responses), Socket);

command({"beat_it", _Msg, User}, Socket) ->
    Responses = [
        " Just Beat It!",
        " Just Eat It!",
        " Eat Grandma!"
    ],
    send_msg("@" ++ User ++ random_choice(Responses), Socket);

command({"quote", _Msg, _User}, Socket) ->
    Responses = [
        "\"hope y'all hungry for sausages :pogChamp:\" @EpicViewer",
        "\"as the author, i don't want credit\" @EpicViewer",
        "\"everything i say is being quoted! FeelsBadMan\" @EpicViewer",
        "\"My AIDs really itch tonight.\" @Hermeneutics",
        "\"Why are all these programmers following me?!\" @EpicViewer",
        "\"How you crap the pid back BrokeBack \" @ws_ubi",
        "\"Just because you're right doesn't mean you get to be right\" @SudoKid",
        "\"The goodest stream\" @ethanpmorgan"
    ],
    send_msg(random_choice(Responses), Socket);

command({"joe_armstrong", _Msg, _User}, Socket) ->
    send_msg("Joe Armstrong is my god!", Socket);

command({"c++", _Msg, _User}, Socket) ->
    send_msg("Why not i386?", Socket);

command({"true_language", _Msg, _User}, Socket) ->
    send_msg("i386 is the one True Language!", Socket);

command({"javascript", _Msg, User}, Socket) ->
    Responses = [
        "@" ++ User ++ " Why do you hate yourself?",
        "Why not wasm?"
    ],
    send_msg(random_choice(Responses), Socket);

command({"php", _Msg, User}, Socket) ->
    send_msg("@" ++ User ++ " Why do you hate yourself?", Socket);

command({"what_you_doing?", _Msg, _User}, Socket) ->
    send_msg("Your mom", Socket);

command({"what_are_you_doing?", _Msg, _User}, Socket) ->
    command({"what_you_doing?", _Msg, _User}, Socket);

command({"whatyoudoing?", _Msg, _User}, Socket) ->
    command({"what_you_doing?", _Msg, _User}, Socket);

command({"epicviewer", _Msg, _User}, Socket) ->
    send_msg("https://www.twitch.tv/epicviewer/", Socket);

command({"ep1cviewr", _Msg, _User}, Socket) ->
    send_msg("Ep1cViewr is an IMPOSTER!!", Socket);

command({"aids", _Msg, User}, Socket) ->
    Responses = [
        "@" ++ User ++ " You have have AIDS!!",
        "@" ++ User ++ " You don't have AIDS!!",
        "@" ++ User ++ " You have AIDS!!",
        "@" ++ User ++ " You AIDS have!!",
        "@" ++ User ++ " You the HEPATITS-AIDS have!!",
        "@" ++ User ++ " You have HEPATITS-AIDS!!",
        "@" ++ User ++ " You have HEPATITIS-AIDS!!",
        "@" ++ User ++ " You have HEPATITIS-CHOLERA-AIDS!!",
        "@" ++ User ++ " You have CHLAMYDIA-AIDS!!",
        "@" ++ User ++ " Du hast AIDS!!",
        "@" ++ User ++ " NOOT AIDS NOOT!!"
        %% "@" ++ User ++ " You have S̸͢p̵͝I̵͟∀̴̵̸̡͡"
    ],
    send_msg(random_choice(Responses), Socket);

command({"sudokid", _Msg, _User}, Socket) ->
    send_msg("SudoKid is a bot!", Socket);

command({"link", Msg, _User}, Socket) ->
    send_msg("https://www.twitch.tv/" ++ Msg, Socket);

command({"hi", _Msg, User}, Socket) ->
    send_msg("Hi " ++ User, Socket);

command({"bye", _Msg, User}, Socket) ->
    send_msg("Bye " ++ User, Socket);

command(_Msg, Socket) ->
    Socket.

