%%%-------------------------------------------------------------------
%%% @author savik
%%% @doc
%%%
%%% @end
%%% Created : 15. Jan 2019 21:09
%%%-------------------------------------------------------------------
-module(token_bucket).
-author("savik").

-behaviour(gen_server).
%% API
-export([start_link/0]).

-export([limit_is_reached/1, limit_is_reached/2]).
-export([remove_old_limiters/1, timestamp/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_RPS, 2000).

-record(raterlimiter, {cleanup_rate, timeout}).
-record(state, {number_of_tokens, max_tokens}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(limit_is_reached(UserId :: term()) -> true | false).
limit_is_reached(UserId) ->
  limit_is_reached(UserId, ?DEFAULT_RPS).

-spec(token_bucket:limit_is_reached(UserId :: term(), MaxRPS :: non_neg_integer()) -> true | false).
limit_is_reached(UserId, MaxRPS) ->
  Stamp = timestamp(),    %% milliseconds
  case mnesia_driver:select_last() of
    empty ->
      mnesia_driver:add_user(UserId, Stamp),
      true;
    {other, Other} ->
      io:format("~nError: ~n~p~n", [Other]),
      false;
    LastInsert ->
      io:format("~nLastInsert: ~n~p~n", [LastInsert]),
      case MaxRPS < (Stamp - LastInsert) of
        true ->
          mnesia_driver:add_user(UserId, Stamp),
          true;
        _ -> false
      end
  end.

-spec remove_old_limiters(Timeout::integer()) -> Number::integer().
%% @doc Removes old counters and returns number of deleted counters.
remove_old_limiters(Timeout) ->
  NowStamp = timestamp(),
  mnesia_driver:remove_fields(NowStamp - Timeout).

-spec timestamp() -> Timstamp::integer().
%% @doc Returns now() as milliseconds
timestamp() ->
  erlang:convert_time_unit(erlang:system_time(), native, millisecond).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("~nStarting applcation Token backet~n"),
  Timeout = 300000,
  Rate = 10000,
  io:format("~nStarting Token backet with Timeout ~p, Cleanup every ~p milliseconds ~n",[Timeout, Rate]),

  mnesia_driver:start(),

  timer:send_interval(Rate, remove_old),
  {ok, #raterlimiter{timeout=Timeout, cleanup_rate=Rate}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Msg, _From, State) ->
  {reply, ok,  State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(remove_old, State)->
  remove_old_limiters(State#raterlimiter.timeout),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================