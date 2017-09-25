%% Copyright 2014 Sean Cribbs
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(capture_io_tests).
-include_lib("eunit/include/eunit.hrl").


basic_output_capture_test() ->
    {ok, Capture} = capture_io:start_link(),
    {ok, <<"">>} = capture_io:get_output(Capture),
    io:format("~s test ", ["some"]),
    io:format("~s test ", ["other"]),
    {ok, <<"some test other test ">>} = capture_io:get_output(Capture),
    ok = capture_io:reset(Capture),
    {ok, <<"">>} = capture_io:get_output(Capture),
    ok = capture_io:stop(Capture),
    ok.


get_stop_test() ->
    {ok, Capture} = capture_io:start_link(),
    MonitorRef = erlang:monitor(process, Capture),
    io:format("~s test", ["this"]),
    {ok, <<"this test">>} = capture_io:get_stop(Capture),
    ok = receive {'DOWN', MonitorRef, process, Capture, _} -> ok after 1000 -> error end,
    ok.


capture_fun_test() ->
    {ok, <<"fun test">>, ok} = capture_io:capture(fun () ->
        io:format("~s test", ["fun"]), ok
    end).


