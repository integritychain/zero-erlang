-author("eschorn").

%% See Appendix A.1 of https://tools.ietf.org/html/rfc5054
-define(PRIME, 16#EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3).
-define(GENERATOR, 16#02).

-define(MAX_ITER, 4).

%% State record
-record(state, {x :: non_neg_integer(), y :: non_neg_integer(),
                r :: non_neg_integer(), c :: non_neg_integer(),
                iter :: non_neg_integer() , honest :: boolean()}).
