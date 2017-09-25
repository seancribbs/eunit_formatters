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


%%
%%  @doc Simple IO device for capturing test output.
%%
-module(capture_io).
-export([start_link/0, get_output/1, get_stop/1, reset/1, stop/1, capture/1]).
-export([init/1, loop/1]).

-record(state, {
    leader,
    output
}).


%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%  Start the capturing process.
%%
start_link() ->
    OldLeader = erlang:group_leader(),
    Pid = spawn_link(?MODULE,init,[OldLeader]),
    true = erlang:group_leader(Pid, self()),
    {ok, Pid}.


%%
%%  Get captured data.
%%
get_output(Pid) ->
    Pid ! {get_output, self()},
    receive
        {output, IoList} ->
            {ok, IoList}
    after 5000 ->
        {error, timeout}
    end.


%%
%%  Get captured data and stop capturing.
%%
get_stop(Pid) ->
    Pid ! {get_stop, self()},
    receive
        {output, IoList, OldLeader} ->
            true = erlang:group_leader(OldLeader, self()),
            {ok, IoList}
    after 5000 ->
        {error, timeout}
    end.


%%
%%  Reset the captured data.
%%
reset(Pid) ->
    Pid ! reset,
    ok.


%%
%%  Stop the IO device.
%%
stop(Pid) ->
    unlink(Pid),
    Pid ! {stop, self()},
    receive
        {stopped, OldLeader} ->
            true = erlang:group_leader(OldLeader, self()),
            ok
    after 5000 ->
        {error, timeout}
    end.


%%
%%  Capture output of a function.
%%
capture(Fun) ->
    {ok, Capture} = start_link(),
    Result = Fun(),
    {ok, Output} = get_stop(Capture),
    {ok, Output, Result}.


%% =============================================================================
%%  IO Device implementation.
%%  See `http://erlang.org/doc/apps/stdlib/io_protocol.html` for more details.
%% =============================================================================

%%
%%  Initialization.
%%
init(OldLeader) ->
    ?MODULE:loop(#state{leader = OldLeader, output = []}).


%%
%%  Main loop.
%%
loop(State = #state{leader = OldLeader, output = Output}) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            case handle_request(Request, State) of
                {reply, Reply, NewState} ->
                    From ! {io_reply, ReplyAs, Reply},
                    ?MODULE:loop(NewState)
            end;
        {get_output, From} ->
            From ! {output, unicode:characters_to_binary(lists:reverse(Output))},
            ?MODULE:loop(State);
        {get_stop, From} ->
            From ! {output, unicode:characters_to_binary(lists:reverse(Output)), OldLeader},
            exit(normal);
        reset ->
            ?MODULE:loop(State#state{output = []});
        {stop, From} ->
            From ! {stopped, OldLeader},
            exit(normal);
        _Unknown ->
            ?MODULE:loop(State)
    end.


%%
%%  Handle output requests.
%%
handle_request({put_chars, _Encoding, Chars}, State = #state{output = Output}) ->
    {reply, ok, State#state{output = [Chars | Output]}};

handle_request({put_chars, Encoding, Module, Function, Args}, State) ->
    handle_request({put_chars, Encoding, apply(Module, Function, Args)}, State);

handle_request({put_chars, Characters}, State) ->
    handle_request({put_chars, latin1, Characters}, State);

handle_request({put_chars, Module, Function, Args}, State) ->
    handle_request({put_chars, latin1, Module, Function, Args}, State);

handle_request(_, State) ->
    {reply, {error, unsupported}, State}.


