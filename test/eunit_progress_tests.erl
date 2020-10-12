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
-module(eunit_progress_tests).
-include_lib("eunit/include/eunit.hrl").


%%
%%  Check if `.` and `F` are printed.
%%
basic_test() ->
    {ok, Output, error} = capture_io:capture(fun () ->
        Tests = [
            fun () -> ?assert(true) end,
            fun () -> ?assert(false) end,
            fun () -> ?assertNot(true) end,
            fun () -> error(some) end
        ],
        eunit:test(Tests, [no_tty, {report, {eunit_progress, [colored, profile]}}])
    end),
    ?assertMatch(match, re:run(Output, "^.*\\..*F.*F.*F.*\nFailures.*", [{capture, none}])).


%%
%%  Check if hamcrest assert failures are handled properly.
%%
hamcrest_assert_test() ->
    {ok, Output, error} = capture_io:capture(fun () ->
        Props = [{matcher, some}, {expected, other}, {actual, this}],
        Test = fun () -> erlang:error({assertion_failed, Props}) end,
        eunit:test(Test, [no_tty, {report, {eunit_progress, [colored, profile]}}])
    end),
    Pattern = "\\?assertThat\\(some\\)(.|\n)*expected:.*other(.|\n)*got:.*this",
    ?assertMatch(match, re:run(Output, Pattern, [{capture, none}])).


%%
%%  Check if unknown assert failures are handled properly.
%%
unknown_assert_test() ->
    {ok, Output, error} = capture_io:capture(fun () ->
        Props = [{my_description, is_much_better}],
        Test = fun () -> erlang:error({assertion_failed, Props}) end,
        eunit:test(Test, [no_tty, {report, {eunit_progress, [colored, profile]}}])
    end),
    Pattern = "unknown assert:(.|\n)*my_description(.|\n)*is_much_better",
    ?assertMatch(match, re:run(Output, Pattern, [{capture, none}])).


%%
%%  Check if exit reason is printed in the case, when a process linked with
%%  the test process is terminated.
%%
print_process_exit_test() ->
    {ok, Output, error} = capture_io:capture(fun () ->
        Proc = fun () -> exit(my_error) end,
        Test = fun () -> erlang:spawn_link(Proc), ok = receive after 100 -> error end end,
        eunit:test(Test, [no_tty, {report, {eunit_progress, [colored, profile]}}])
    end),
    Pattern = "Related process exited with reason(.|\n)*my_error",
    ?assertMatch(match, re:run(Output, Pattern, [{capture, none}])).

%%
%% Unicode binaries and atoms should print correctly. We are not
%% setting the console IO option for utf8, but at least we won't crash
%% on badarg. For example, instead of the "ą" in the test, you will
%% see << "\x{105}" / utf8 >> printed.
%%
print_unicode_values_test() ->
    {ok, Output, error} = capture_io:capture(fun () ->
        Test = ?_assertMatch(<<"ą"/utf8>>, <<"a"/utf8>>),
        eunit:test(Test, [no_tty, {report, {eunit_progress, [colored, profile]}}])
    end),
    io:format("%%%%%%%%%%%%%%%%~n~ts~n%%%%%%%%%%%%%%%%~n", [Output]),
    Pattern = "\\{badarg,\\[\\{io_lib,format,",
    ?assertMatch(nomatch, re:run(Output, Pattern, [{capture, none}, unicode])).
