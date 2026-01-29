%-*- mode: erlang -*-
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(hlc_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ts(W, L), {timestamp, W, L}).

basic_test() ->
  {ok, C} = hlc:start_link(fun hlc:physical_clock/0, 0),
  S = hlc:now(C),
  timer:sleep(5),
  T = ?ts(hlc:physical_clock(), 0),

  ?assertMatch(true, hlc:less(S, T)),
  ?assertMatch(false, hlc:less(T, S)),
  ?assert(element(2, T) > element(2, S)),
  ?assert(element(3, S) =:= 0),
  hlc:stop(C).

manual_clock_test() ->
  {Pid, Fun} = hlc:manual_clock(),
  ?assert(Fun() =:= 0),
  ok = hlc:set_manual_clock(Pid, 1),
  ?assert(Fun() =:= 1),
  ok = hlc:set_manual_clock(Pid, 2),
  ?assert(Fun() =:= 2),
  hlc:stop_manual_clock(Pid).

less_test() ->
  {MClock, MClockFun} = hlc:manual_clock(),
  {ok, C} = hlc:start_link(MClockFun, 0),

  A = hlc:timestamp(C),
  B = hlc:timestamp(C),

  ?assert(A =:= B),

  hlc:set_manual_clock(MClock, 1),
  B1 = hlc:now(C),
  ?assertMatch(true, hlc:less(A, B1)),
  hlc:stop(C),
  hlc:stop_manual_clock(MClock).

equal_test() ->
  {MClock, MClockFun} = hlc:manual_clock(),
  {ok, C} = hlc:start_link(MClockFun, 0),

  A = hlc:timestamp(C),
  B = hlc:timestamp(C),

  ?assertMatch(true, hlc:equal(A, B)),

  hlc:set_manual_clock(MClock, 1),
  B1 = hlc:now(C),
  ?assertMatch(false, hlc:equal(A, B1)),
  hlc:stop(C),
  hlc:stop_manual_clock(MClock).

clock_test() ->
  error_logger:tty(false),
  {MClock, MClockFun} = hlc:manual_clock(),
  {ok, C} = hlc:start_link(MClockFun, 0),
  ok = hlc:set_maxoffset(1000, C),

  Cases = [{5, send, nil, ?ts(5,0)},
           {6, send, nil, ?ts(6,0)},
           {10, recv, ?ts(10,5), ?ts(10, 6)},
           {7, send, nil, ?ts(10, 7)},
           {8, recv, ?ts(10, 4), ?ts(10, 8)},
           {9, recv, ?ts(1100, 888), ?ts(10, 8)},
           {10, recv, ?ts(10, 99), ?ts(10, 100)},
           {11, recv, ?ts(10, 31), ?ts(11, 0)},
           {11, send, nil, ?ts(11, 1)}],

  _ = lists:foreach(fun
                    ({WallClock, send, _Input, Expected}) ->
                      hlc:set_manual_clock(MClock, WallClock),
                      Current = hlc:now(C),
                      ?assertMatch(Current, Expected);
                    ({WallClock, recv, Input, Expected}) ->
                      hlc:set_manual_clock(MClock, WallClock),
                      Previous = hlc:timestamp(C),
                      case hlc:update(C, Input) of
                        {timeahead, Current} ->
                          ?assertMatch(Current, Expected);
                        {ok, Current} ->
                          ?assert(Current /= Previous),
                          ?assertMatch(Current, Expected)
                      end
                  end, Cases),
  hlc:stop(C),
  hlc:stop_manual_clock(MClock),
  error_logger:tty(true).

set_maxoffset_test() ->
  error_logger:tty(false),
  {MClock, MClockFun} = hlc:manual_clock(123456789),
  SkewedTime = 123456789 + 51,
  {ok, C} = hlc:start_link(MClockFun, 0),

  ?assert(hlc:get_maxoffset(C) =:= 0),
  ok = hlc:set_maxoffset(50, C),
  ?assert(hlc:get_maxoffset(C) =:= 50),

  _ = hlc:now(C),
  TS = hlc:timestamp(C),
  ?assert(element(2, TS) =:= 123456789),

  ?assertMatch({timeahead, _}, hlc:update(C, ?ts(SkewedTime, 0))),

  ok = hlc:set_maxoffset(0, C),
  ?assertMatch({ok, {timestamp, SkewedTime, _}},
               hlc:update(C, ?ts(SkewedTime, 0))),
  hlc:stop(C),
  hlc:stop_manual_clock(MClock),
  error_logger:tty(true).
