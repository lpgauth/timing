timing
======

#### Function Details: ####

##### timing:function/1 ######

    function(fun()) -> term().

##### timing:function/2 ######

    function(fun(), pos_integer()) -> term().

##### timing:function/3 ######

    function(fun(), pos_integer(), pos_integer()) -> term().

#### Example: ####

    timing:function(fun() -> os:timestamp() end).
    [{min,0},
     {max,2285},
     {arithmetic_mean,3.9076164002779707},
     {geometric_mean,1.4134280005426059},
     {harmonic_mean,1.3307011069525767},
     {median,1},
     {variance,3245.543689645705},
     {standard_deviation,56.9696734205639},
     {skewness,26.858502344237007},
     {kurtosis,782.8947599926026},
     {percentile,[{50,1},{75,2},{90,2},{95,2},{99,5},{999,1154}]},
     {histogram,[{5,71330},
                 {10,236},
                 {15,75},
                 {20,16},
                 {25,28},
                 {30,25},
                 {40,16},
                 {50,4},
                 {60,5},
                 {70,9},
                 {80,2},
                 {90,3},
                 {100,1},
                 {110,...},
                 {...}|...]},
     {n,71950}]

