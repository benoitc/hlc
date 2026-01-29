%-*- mode: erlang -*-
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(hlc_harness).
-author("benoitc").

%% API
-export([
  generate/1,
  timed_generate/1
]).

-include_lib("eunit/include/eunit.hrl").


generate(N) ->
  {ok, Clock} = hlc:start_link(fun hlc:physical_clock/0, 0),
  [hlc:now(Clock) || _I <- lists:seq(1, N)].

timed_generate(N) ->
  ?debugTime("generating timestamp", generate(N)).
