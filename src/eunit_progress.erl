%% @doc A listener/reporter for eunit that prints '.' for each
%% success, 'F' for each failure, and 'E' for each error. It can also
%% optionally summarize the failures at the end.
-module(eunit_progress).
-behaviour(eunit_listener).
-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

-define(RED, "\e[0;31m").
-define(GREEN, "\e[0;32m").
-define(YELLOW, "\e[0;33m").
-define(WHITE, "\e[0;37m").
-define(CYAN, "\e[0;36m").
-define(RESET, "\e[0m").

%% eunit_listener callbacks
-export([
         init/1,
         handle_begin/3,
         handle_end/3,
         handle_cancel/3,
         terminate/2
        ]).

-export([
         start/0,
         start/1
        ]).

-ifdef(namespaced_dicts).
-type euf_dict() :: dict:dict().
-else.
-type euf_dict() :: dict().
-endif.

-record(state, {
          status = dict:new() :: euf_dict(),
          failures = [] :: [[pos_integer()]],
          skips = [] :: [[pos_integer()]],
          timings = binomial_heap:new() :: binomial_heap:binomial_heap(),
          colored = true :: boolean(),
          profile = false :: boolean()
         }).

%% Startup
start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

%%------------------------------------------
%% eunit_listener callbacks
%%------------------------------------------
init(Options) ->
    #state{colored=proplists:get_bool(colored, Options),
           profile=proplists:get_bool(profile, Options)}.

handle_begin(group, Data, St) ->
    GID = proplists:get_value(id, Data),
    Dict = St#state.status,
    St#state{status=dict:store(GID, orddict:from_list([{type, group}|Data]), Dict)};
handle_begin(test, Data, St) ->
    TID = proplists:get_value(id, Data),
    Dict = St#state.status,
    St#state{status=dict:store(TID, orddict:from_list([{type, test}|Data]), Dict)}.

handle_end(group, Data, St) ->
    St#state{status=merge_on_end(Data, St#state.status)};
handle_end(test, Data, St) ->
    NewStatus = merge_on_end(Data, St#state.status),
    St1 = print_progress(Data, St),
    St2 = record_timing(Data, St1),
    St2#state{status=NewStatus}.

handle_cancel(_, Data, #state{status=Status, skips=Skips}=St) ->
    Status1 = merge_on_end(Data, Status),
    ID = proplists:get_value(id, Data),
    St#state{status=Status1, skips=[ID|Skips]}.

terminate({ok, Data}, St) ->
    print_failures(St),
    print_pending(St),
    print_profile(St),
    print_timing(St),
    print_results(Data, St);
terminate({error, Reason}, St) ->
    io:nl(), io:nl(),
    print_colored(io_lib:format("Eunit failed: ~25p~n", [Reason]), ?RED, St),
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

%%------------------------------------------
%% Print and collect information during run
%%------------------------------------------
print_progress(Data, St) ->
    TID = proplists:get_value(id, Data),
    case proplists:get_value(status, Data) of
        ok ->
            print_progress_success(St),
            St;
        {skipped, _Reason} ->
            print_progress_skipped(St),
            St#state{skips=[TID|St#state.skips]};
        {error, Exception} ->
            print_progress_failed(Exception, St),
            St#state{failures=[TID|St#state.failures]}
    end.

record_timing(Data, State=#state{timings=T, profile=true}) ->
    TID = proplists:get_value(id, Data),
    case lists:keyfind(time, 1, Data) of
        {time, Int} ->
            %% It's a min-heap, so we insert negative numbers instead
            %% of the actuals and normalize when we report on them.
            T1 = binomial_heap:insert(-Int, TID, T),
            State#state{timings=T1};
        false ->
            State
    end;
record_timing(_Data, State) ->
    State.

print_progress_success(St) ->
    print_colored(".", ?GREEN, St).

print_progress_skipped(St) ->
    print_colored("*", ?YELLOW, St).

print_progress_failed(_Exc, St) ->
    print_colored("F", ?RED, St).

merge_on_end(Data, Dict) ->
    ID = proplists:get_value(id, Data),
    dict:update(ID,
                fun(Old) ->
                        orddict:merge(fun merge_data/3, Old, orddict:from_list(Data))
                end, Dict).

merge_data(_K, undefined, X) -> X;
merge_data(_K, X, undefined) -> X;
merge_data(_K, _, X) -> X.

%%------------------------------------------
%% Print information at end of run
%%------------------------------------------
print_failures(#state{failures=[]}) ->
    ok;
print_failures(#state{failures=Fails}=State) ->
    io:nl(),
    io:fwrite("Failures:~n~n",[]),
    lists:foldr(print_failure_fun(State), 1, Fails),
    ok.

print_failure_fun(#state{status=Status}=State) ->
    fun(Key, Count) ->
            TestData = dict:fetch(Key, Status),
            TestId = format_test_identifier(TestData),
            io:fwrite("  ~p) ~s~n", [Count, TestId]),
            print_failure_reason(proplists:get_value(status, TestData),
                                 proplists:get_value(output, TestData),
                                 State),
            io:nl(),
            Count + 1
    end.

print_failure_reason({skipped, Reason}, _Output, State) ->
    print_colored(io_lib:format("     ~s~n", [format_pending_reason(Reason)]),
                  ?RED, State);
print_failure_reason({error, {_Class, Term, Stack}}, Output, State) when
      is_tuple(Term), tuple_size(Term) == 2, is_list(element(2, Term)) ->
    print_assertion_failure(Term, Stack, Output, State),
    print_failure_output(5, Output, State);
print_failure_reason({error, Reason}, Output, State) ->
    print_colored(indent(5, "Failure/Error: ~p~n", [Reason]), ?RED, State),
    print_failure_output(5, Output, State).

print_failure_output(_, <<>>, _) -> ok;
print_failure_output(_, undefined, _) -> ok;
print_failure_output(Indent, Output, State) ->
    print_colored(indent(Indent, "Output: ~s", [Output]), ?CYAN, State).

print_assertion_failure({Type, Props}, Stack, Output, State) ->
    FailureDesc = format_assertion_failure(Type, Props, 5),
    {M,F,A,Loc} = lists:last(Stack),
    LocationText = io_lib:format("     %% ~s:~p:in `~s`", [proplists:get_value(file, Loc),
                                                           proplists:get_value(line, Loc),
                                                           format_function_name(M,F,A)]),
    print_colored(FailureDesc, ?RED, State),
    io:nl(),
    print_colored(LocationText, ?CYAN, State),
    io:nl(),
    print_failure_output(5, Output, State),
    io:nl().

print_pending(#state{skips=[]}) ->
    ok;
print_pending(#state{status=Status, skips=Skips}=State) ->
    io:nl(),
    io:fwrite("Pending:~n", []),
    lists:foreach(fun(ID) ->
                          Info = dict:fetch(ID, Status),
                          case proplists:get_value(reason, Info) of
                              undefined ->
                                  ok;
                              Reason ->
                                  print_pending_reason(Reason, Info, State)
                          end
                  end, lists:reverse(Skips)),
    io:nl().

print_pending_reason(Reason0, Data, State) ->
    Text = case proplists:get_value(type, Data) of
               group ->
                   io_lib:format("  ~s~n", [proplists:get_value(desc, Data)]);
               test ->
                   io_lib:format("  ~s~n", [format_test_identifier(Data)])
           end,
    Reason = io_lib:format("    %% ~s~n", [format_pending_reason(Reason0)]),
    print_colored(Text, ?YELLOW, State),
    print_colored(Reason, ?CYAN, State).

print_profile(#state{timings=T, status=Status, profile=true}=State) ->
    TopN = binomial_heap:take(10, T),
    TopNTime = abs(lists:sum([ Time || {Time, _} <- TopN ])),
    TLG = dict:fetch([], Status),
    TotalTime = proplists:get_value(time, TLG),
    if TLG > 0 andalso TopN =/= [] andalso TotalTime =/= undefined ->
            TopNPct = (TopNTime / TotalTime) * 100,
            io:nl(), io:nl(),
            io:fwrite("Top ~p slowest tests (~s, ~.1f% of total time):", [length(TopN), format_time(TopNTime), TopNPct]),
            lists:foreach(print_timing_fun(State), TopN),
            io:nl();
       true -> ok
    end;
print_profile(#state{profile=false}) ->
    ok.

print_timing(#state{status=Status}) ->
    TLG = dict:fetch([], Status),
    Time = proplists:get_value(time, TLG),
    io:nl(),
    io:fwrite("Finished in ~s~n", [format_time(Time)]),
    ok.

print_results(Data, State) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    Total = Pass + Fail + Skip + Cancel,
    {Color, Result} = if Fail > 0 -> {?RED, error};
                         Skip > 0; Cancel > 0 -> {?YELLOW, error};
                         Pass =:= 0 -> {?YELLOW, ok};
                         true -> {?GREEN, ok}
                      end,
    print_results(Color, Total, Fail, Skip, Cancel, State),
    sync_end(Result).

print_results(Color, 0, _, _, _, State) ->
    print_colored(Color, "0 tests\n", State);
print_results(Color, Total, Fail, Skip, Cancel, State) ->
    SkipText = format_optional_result(Skip, "skipped"),
    CancelText = format_optional_result(Cancel, "cancelled"),
    Text = io_lib:format("~p tests, ~p failures~s~s~n", [Total, Fail, SkipText, CancelText]),
    print_colored(Text, Color, State).

print_timing_fun(#state{status=Status}=State) ->
    fun({Time, Key}) ->
            TestData = dict:fetch(Key, Status),
            TestId = format_test_identifier(TestData),
            io:nl(),
            io:fwrite("  ~s~n", [TestId]),
            print_colored(["    "|format_time(abs(Time))], ?RED, State)
    end.

%%------------------------------------------
%% Print to the console with the given color
%% if enabled.
%%------------------------------------------
print_colored(Text, Color, #state{colored=true}) ->
    io:fwrite("~s~s~s", [Color, Text, ?RESET]);
print_colored(Text, _Color, #state{colored=false}) ->
    io:fwrite("~s", [Text]).

%%------------------------------------------
%% Generic data formatters
%%------------------------------------------
format_function_name(M, F, A) ->
    io_lib:format("~s:~s/~p", [M, F, A]).

format_optional_result(0, _) ->
    [];
format_optional_result(Count, Text) ->
    io_lib:format(", ~p ~s", [Count, Text]).

format_test_identifier(Data) ->
    {Mod, Fun, Arity} = proplists:get_value(source, Data),
    Line = case proplists:get_value(line, Data) of
               0 -> "";
               L -> io_lib:format(":~p", [L])
           end,
    Desc = case proplists:get_value(desc, Data) of
               undefined ->  "";
               DescText -> io_lib:format(": ~s", [DescText])
           end,
    io_lib:format("~s~s~s", [format_function_name(Mod, Fun, Arity), Line, Desc]).

format_time(undefined) ->
    "? seconds";
format_time(Time) ->
    io_lib:format("~.3f seconds", [Time / 1000]).

format_pending_reason({module_not_found, M}) ->
    io_lib:format("Module '~s' missing", [M]);
format_pending_reason({no_such_function, {M,F,A}}) ->
    io_lib:format("Function ~s undefined", [format_function_name(M,F,A)]);
format_pending_reason({exit, Reason}) ->
    io_lib:format("Related process exited with reason: ~p", [Reason]);
format_pending_reason(Reason) ->
    io_lib:format("Unknown error: ~p", [Reason]).

%% @doc Formats all the known eunit assertions, you're on your own if
%% you make an assertion yourself.
format_assertion_failure(assertion_failed, Props, I) ->
    Keys = proplists:get_keys(Props),
    HasEUnitProps = ([expression, value] -- Keys) =:= [],
    HasHamcrestProps = ([expected, actual, matcher] -- Keys) =:= [],
    if
        HasEUnitProps ->
            [indent(I, "Failure/Error: ?assert(~s)~n", [proplists:get_value(expression, Props)]),
             indent(I, "  expected: true~n", []),
             case proplists:get_value(value, Props) of
                 false ->
                     indent(I, "       got: false", []);
                 {not_a_boolean, V} ->
                     indent(I, "       got: ~p", [V])
             end];
        HasHamcrestProps ->
            [indent(I, "Failure/Error: ?assertThat(~p)~n", [proplists:get_value(matcher, Props)]),
             indent(I, "  expected: ~p~n", [proplists:get_value(expected, Props)]),
             indent(I, "       got: ~p", [proplists:get_value(actual, Props)])];
        true ->
            [indent(I, "Failure/Error: unknown assert: ~p", [Props])]
    end;

format_assertion_failure(assertMatch_failed, Props, I) ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [indent(I, "Failure/Error: ?assertMatch(~s, ~s)~n", [Pattern, Expr]),
     indent(I, "  expected: = ~s~n", [Pattern]),
     indent(I, "       got: ~p", [Value])];
format_assertion_failure(assertNotMatch_failed, Props, I) ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [indent(I, "Failure/Error: ?assertNotMatch(~s, ~s)~n", [Pattern, Expr]),
     indent(I, "  expected not: = ~s~n", [Pattern]),
     indent(I, "           got:   ~p", [Value])];

format_assertion_failure(assertEqual_failed, Props, I) ->
    Expr = proplists:get_value(expression, Props),
    Expected = proplists:get_value(expected, Props),
    Value = proplists:get_value(value, Props),
    [indent(I, "Failure/Error: ?assertEqual(~w, ~s)~n", [Expected,
                                                         Expr]),
     indent(I, "  expected: ~p~n", [Expected]),
     indent(I, "       got: ~p", [Value])];
format_assertion_failure(assertNotEqual_failed, Props, I) ->
    Expr = proplists:get_value(expression, Props),
    Value = proplists:get_value(value, Props),
    [indent(I, "Failure/Error: ?assertNotEqual(~p, ~s)~n",
            [Value, Expr]),
     indent(I, "  expected not: == ~p~n", [Value]),
     indent(I, "           got:    ~p", [Value])];

format_assertion_failure(assertException_failed, Props, I) ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DATA
    [indent(I, "Failure/Error: ?assertException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     case proplists:is_defined(unexpected_success, Props) of
         true ->
             [indent(I, "  expected: exception ~s but nothing was raised~n", [Pattern]),
              indent(I, "       got: value ~p", [proplists:get_value(unexpected_success, Props)])];
         false ->
             Ex = proplists:get_value(unexpected_exception, Props),
             [indent(I, "  expected: exception ~s~n", [Pattern]),
              indent(I, "       got: exception ~p", [Ex])]
     end];
format_assertion_failure(assertNotException_failed, Props, I) ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DAT
    Ex = proplists:get_value(unexpected_exception, Props),
    [indent(I, "Failure/Error: ?assertNotException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     indent(I, "  expected not: exception ~s~n", [Pattern]),
     indent(I, "           got: exception ~p", [Ex])];

format_assertion_failure(command_failed, Props, I) ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [indent(I, "Failure/Error: ?cmdStatus(~p, ~p)~n", [Expected, Cmd]),
     indent(I, "  expected: status ~p~n", [Expected]),
     indent(I, "       got: status ~p", [Status])];

format_assertion_failure(assertCmd_failed, Props, I) ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [indent(I, "Failure/Error: ?assertCmdStatus(~p, ~p)~n", [Expected, Cmd]),
     indent(I, "  expected: status ~p~n", [Expected]),
     indent(I, "       got: status ~p", [Status])];

format_assertion_failure(assertCmdOutput_failed, Props, I) ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_output, Props),
    Output = proplists:get_value(output, Props),
    [indent(I, "Failure/Error: ?assertCmdOutput(~p, ~p)~n", [Expected, Cmd]),
     indent(I, "  expected: ~p~n", [Expected]),
     indent(I, "       got: ~p", [Output])];

format_assertion_failure(Type, Props, I) ->
    indent(I, "~p", [{Type, Props}]).

indent(I, Fmt, Args) ->
    io_lib:format("~" ++ integer_to_list(I) ++ "s" ++ Fmt, [" "|Args]).

extract_exception_pattern(Str) ->
    ["{", Class, Term|_] = string:tokens(Str, ", "),
    {Class, Term}.
