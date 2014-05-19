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


