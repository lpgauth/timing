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
    T1 = erlang:monotonic_time(),
    spawn_loop(Concurrency, TimingFun, N, self(), SpawnOpts),
    {ok, Timings} = receive_timings(Concurrency, []),
    T2 = erlang:monotonic_time(),
    TotalTime = erlang:convert_time_unit(T2 - T1, native, microsecond),
    {ok, Errors} = timings_loop(lists:flatten(Timings), Hdr, 0),
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
        {stddev, hdr_histogram:stddev(Hdr)},
        {p95, hdr_histogram:percentile(Hdr, 95.0)},
        {p99, hdr_histogram:percentile(Hdr, 99.0)},
        {p999, hdr_histogram:percentile(Hdr, 99.9)}
    ],

    hdr_histogram:close(Hdr),
    Result.

%% private
fun_loop(_Fun, 0) ->
    [];
fun_loop(Fun, N) ->
    [fun_time(Fun) | fun_loop(Fun, N - 1)].

fun_time(Fun) ->
    T1 = erlang:monotonic_time(),
    case Fun() of
        ok ->
            T2 = erlang:monotonic_time(),
            TDiff = erlang:convert_time_unit(T2 - T1, native, microsecond),
            {ok, TDiff};
        {error, _} = Error ->
            Error
    end.

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

receive_timings(0, Timings) ->
    {ok, Timings};
receive_timings(C, Timings) ->
    receive
        {'EXIT', _Pid, normal} ->
            receive_timings(C, Timings);
        {timings, Timings2} ->
            receive_timings(C - 1, [Timings2 | Timings])
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
