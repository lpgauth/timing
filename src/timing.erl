-module(timing).

-export([
    function/1,
    function/2,
    function/3,
    function/4
]).

-define(N, 20000).
-define(P, 20).

%% public
function(Fun) ->
    function(Fun, ?N).

function(Fun, N) ->
    function(Fun, N, ?P).

function(Fun, N, P) ->
    function(Fun, N, P, []).

function(Fun, N, P, Opts) ->
    I = trunc(N/P),
    function_spawn_loop(self(), Fun, I, P, Opts),
    Samples = lists:append(receive_loop(P)),
    bear:get_statistics(Samples).

%% private
function_loop(_Fun, 0) ->
    [];
function_loop(Fun, I) ->
    [function_time(Fun) | function_loop(Fun, I - 1)].

function_spawn_loop(_Pid, _Fun, _I, 0, _Opts) ->
    ok;
function_spawn_loop(Pid, Fun, I, P, Opts) ->
    spawn_opt(fun () -> Pid ! function_loop(Fun, I) end, [link] ++ Opts),
    function_spawn_loop(Pid, Fun, I, P - 1, Opts).

function_time(Fun) ->
    Timestamp = os:timestamp(),
    Fun(),
    timer:now_diff(os:timestamp(), Timestamp).

receive_loop(0) ->
    [];
receive_loop(N) ->
    [receive X -> X end | receive_loop(N - 1)].
