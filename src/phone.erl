%%%-------------------------------------------------------------------
%%% @author iiwaasnet
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2019 18:29
%%%-------------------------------------------------------------------
-module(phone).
-author("iiwaasnet").

-behaviour(gen_statem).

%% API
-export([start_link/0,
  liftup_receiver/0,
  put_receiver/0,
  dial/1]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  receiver_down/3,
  receiver_up/3,
  busy/3,
  talking/3,
  connecting/3,
  %handle_event/4,
  terminate/3,
  %%code_change/4,
  callback_mode/0
]).

-define(SERVER, ?MODULE).

-record(state, {dial_num :: []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

liftup_receiver() ->
  gen_statem:cast(?SERVER, liftup_receiver).

put_receiver() ->
  gen_statem:cast(?SERVER, put_receiver).

dial(Number) ->
  gen_statem:cast(?SERVER, {dial, Number}).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  {ok, receiver_down, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
  [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  #{state_name => _StateName, state => _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
receiver_down(enter, _, State) ->
  {keep_state, State#state{dial_num = []}};
receiver_down(cast, liftup_receiver, State) ->
  {next_state, receiver_up, State};
receiver_down(cast, Event, _) ->
  io:fwrite("Event ~p cant be process while receiver is down.~n", [Event]),
  keep_state_and_data.


receiver_up(enter, _, _) ->
  {keep_state_and_data, [{state_timeout, 10000, busy}]};
receiver_up(state_timeout, busy, State) ->
  {next_state, busy, State};
receiver_up(cast, put_receiver, _) ->
  {next_state, receiver_down};
receiver_up(cast, {dial, Number}, #state{dial_num = DialNumber} = State) ->
  PhoneNumber = DialNumber ++ [Number],
  case check_number(PhoneNumber) of
    incomplete ->
      {keep_state, State#state{dial_num = PhoneNumber}, [{state_timeout, 5000, busy}]};
    invalid ->
      {next_state, busy};
    valid ->
      {next_state, connecting, State#state{dial_num = PhoneNumber}}
  end.

connecting(enter, _, #state{dial_num = PhoneNumber}) ->
  io:fwrite("Connecting to: ~s~n", [PhoneNumber]),
  {keep_state_and_data, [{state_timeout, 3000, talking}]};
connecting(state_timeout, talking, State) ->
  {next_state, talking, State}.

talking(enter, _, #state{dial_num = PhoneNumber}) ->
  io:fwrite("Talking to: ~s~n", [PhoneNumber]),
  keep_state_and_data;
talking(cast, put_receiver, State) ->
  {next_state, receiver_down, State}.

busy(enter, _, _) ->
  io:fwrite("Line is busy..."),
  keep_state_and_data;
busy(cast, put_receiver, State) ->
  {next_state, receiver_down, State};
busy(cast, Event, _) ->
  io:fwrite("Event ~p cant be process while busy.~n", [Event]),
  keep_state_and_data.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%%handle_event(_EventType, _EventContent, _StateName, State) ->
%%  NextStateName = the_next_state_name,
%%  {next_state, NextStateName, State}.
%%
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
%%code_change(_OldVsn, StateName, State, _Extra) ->
%%  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_number(PhoneNumber) ->
  if
    length(PhoneNumber) =:= 6 ->
      valid;
    length(PhoneNumber) < 6 ->
      incomplete;
    length(PhoneNumber) > 6 ->
      invalid
  end.