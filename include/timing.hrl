%% defaults
-define(DEFAULT_CONCURRENCY, 20).
-define(DEFAULT_ITERATIONS, 20000).
-define(DEFAULT_NAME, timing_hdr).
-define(DEFAULT_OUTPUT, "timing.hgrm").

%% types
-type name() :: atom().
-type option() :: {concurrency, pos_integer()} |
                  {iterations, pos_integer()} |
                  {output, file:name()} |
                  {spawn_opts, [erlang:spawn_opt_option()]}.

-type options() :: [option()].
-type result() :: [{name, name()} |
                   {iterations, pos_integer()} |
                   {concurency, pos_integer()} |
                   {errors, non_neg_integer()} |
                   {mean, pos_integer()} |
                   {median, pos_integer()} |
                   {stddev, pos_integer()}].

-type timing_fun() :: fun(() -> timing_return()).
-type timing_return() :: ok | {error, term()}.
