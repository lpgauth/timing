-module(timing_hdr).
-include("timing.hrl").

-export([
    run/1,
    run/2
]).

%% public

-spec run(timing_fun()) ->
    result().

run(TimingFun) ->
    run(TimingFun, []).

-spec run(timing_fun(), options()) ->
    result().

run(TimingFun, Opts) ->
    Name = lookup(name, Opts, ?DEFAULT_NAME),
    Iterations = lookup(iterations, Opts, ?DEFAULT_ITERATIONS),
    Concurrency = lookup(concurrency, Opts, ?DEFAULT_CONCURRENCY),
    Output = lookup(output, Opts, ?DEFAULT_OUTPUT),
    SpawnOpts = lookup(spawn_opts, Opts, []),

    N = trunc(Iterations / Concurrency),
    Iterations2 = N * Concurrency,
    {ok, Hdr} = hdr_histogram:open(Iterations2, 3),
    Timestamp = os:timestamp(),
    spawn_loop(Concurrency, TimingFun, N, self(), SpawnOpts),
    {ok, Errors} = receive_timings(Concurrency, Hdr, 0),
    TotalTime = timer:now_diff(os:timestamp(), Timestamp),
    hdr_histogram:log(Hdr, classic, Output),

    Result = [
        {name, Name},
        {iterations, Iterations2},
        {concurrency, Concurrency},
        {success, Iterations2 - Errors},
        {errors, Errors},
        {total_time, TotalTime},
        {mean, hdr_histogram:mean(Hdr)},
        {median, hdr_histogram:median(Hdr)},
        {stddev, hdr_histogram:stddev(Hdr)}
    ],

    hdr_histogram:close(Hdr),
    Result.

%% private
fun_loop(_Fun, 0) ->
    [];
fun_loop(Fun, N) ->
    [fun_time(Fun) | fun_loop(Fun, N - 1)].

fun_time(Fun) ->
    Timestamp = os:timestamp(),
    case Fun() of
        ok ->
            {ok, timer:now_diff(os:timestamp(), Timestamp)};
        {error, _} = Error ->
            Error
    end.

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

receive_timings(0, _Hdr, Errors) ->
    {ok, Errors};
receive_timings(C, Hdr, Errors) ->
    receive
        {'EXIT', _Pid, normal} ->
            receive_timings(C, Hdr, Errors);
        {timings, Timings} ->
            {ok, Errors2} = timings_loop(Timings, Hdr, Errors),
            receive_timings(C - 1, Hdr, Errors2)
    end.

spawn_loop(0, _Fun, _N, _Pid, _SpawnOpts) ->
    ok;
spawn_loop(C, Fun, N, Pid, SpawnOpts) ->
    spawn_opt(fun () ->
        Pid ! {timings, fun_loop(Fun, N)}
    end, [link] ++ SpawnOpts),
    spawn_loop(C - 1, Fun, N, Pid, SpawnOpts).

timings_loop([], _Hdr, Errors) ->
    {ok, Errors};
timings_loop([{ok, Timing} | T], Hdr, Errors) ->
    hdr_histogram:record(Hdr, Timing),
    timings_loop(T, Hdr, Errors);
timings_loop([{error, _} | T], Hdr, Errors) ->
    timings_loop(T, Hdr, Errors + 1).
