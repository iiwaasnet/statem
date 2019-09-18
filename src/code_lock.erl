%%%-------------------------------------------------------------------
%%% @author iiwaasnet
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2019 18:34
%%%-------------------------------------------------------------------
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock_2).

-export([start_link/1,stop/0]).
-export([down/1,up/1,code_length/0]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

start_link(Code) ->
  gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
stop() ->
  gen_statem:stop(?NAME).

down(Button) ->
  gen_statem:cast(?NAME, {down,Button}).
up(Button) ->
  gen_statem:cast(?NAME, {up,Button}).
code_length() ->
  gen_statem:call(?NAME, code_length).
init(Code) ->
  process_flag(trap_exit, true),
  Data = #{code => Code, length => length(Code), buttons => []},
  {ok, locked, Data}.

callback_mode() ->
  [state_functions,state_enter].

-define(HANDLE_COMMON,
  ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).
%%
handle_common(cast, {down,Button}, Data) ->
  {keep_state, Data#{button => Button}};
handle_common(cast, {up,Button}, Data) ->
  case Data of
    #{button := Button} ->
      {keep_state, maps:remove(button, Data),
        [{next_event,internal,{button,Button}}]};
    #{} ->
      keep_state_and_data
  end;
handle_common({call,From}, code_length, #{code := Code}) ->
  {keep_state_and_data,
    [{reply,From,length(Code)}]}.
locked(enter, _OldState, Data) ->
  do_lock(),
  {keep_state, Data#{buttons := []}};
locked(state_timeout, button, Data) ->
  {keep_state, Data#{buttons := []}};
locked(
    internal, {button,Button},
    #{code := Code, length := Length, buttons := Buttons} = Data) ->
  NewButtons =
    if
      length(Buttons) < Length ->
        Buttons;
      true ->
        tl(Buttons)
    end ++ [Button],
  if
    NewButtons =:= Code -> % Correct
      {next_state, open, Data};
    true -> % Incomplete | Incorrect
      {keep_state, Data#{buttons := NewButtons},
        [{state_timeout,30000,button}]} % Time in milliseconds
  end;
?HANDLE_COMMON.
open(enter, _OldState, _Data) ->
  do_unlock(),
  {keep_state_and_data,
    [{state_timeout,10000,lock}]}; % Time in milliseconds
open(state_timeout, lock, Data) ->
  {next_state, locked, Data};
open(internal, {button,_}, _) ->
  {keep_state_and_data, [postpone]};
?HANDLE_COMMON.

do_lock() ->
  io:format("Locked~n", []).
do_unlock() ->
  io:format("Open~n", []).

terminate(_Reason, State, _Data) ->
  State =/= locked andalso do_lock(),
  ok.