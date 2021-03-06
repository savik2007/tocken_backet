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
-define(TIMEOUT, 300000).
-define(RATE, 10000).

-record(remove_old, {cleanup_rate, timeout}).
-record(state, {number_of_tokens, max_tokens}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(limit_is_reached(UserId :: term()) -> true | false).
limit_is_reached(UserId) ->
  limit_is_reached(UserId, ?DEFAULT_RPS).

-spec(token_bucket:limit_is_reached(UserId :: term(), MaxRPS :: non_neg_integer()) -> true | false).
limit_is_reached(UserId, MaxRPS) ->
  Stamp = timestamp(),
  Res =
    case mnesia_driver:select_last() of
      {ok, empty} -> mnesia_driver:add_user(UserId, Stamp);
      {ok, LastInsert} ->
        case MaxRPS < (Stamp - LastInsert) of
          true -> mnesia_driver:add_user(UserId, Stamp);
          Other -> {false, Other}
        end;
      Error -> Error
    end,
  check_result(Res).

-spec(check_result(Res::tuple()) -> true | false).
check_result({atomic, _}) -> true;
check_result(_) -> false.

-spec remove_old_limiters(Timeout::integer()) -> ok.
remove_old_limiters(Timeout) ->
  NowStamp = timestamp(),
  mnesia_driver:remove_fields(NowStamp - Timeout).

-spec timestamp() -> Timstamp::integer().
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
-spec(init(Args :: []) ->
  {ok, State :: #remove_old{}} | {ok, State :: #remove_old{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("~nStarting applcation Token backet~n"),
  mnesia_driver:start(),
  {ok, _} = timer:send_interval(?RATE, remove_old),
  io:format("~nStarting Token backet with Timeout ~p, Cleanup every ~p milliseconds ~n",[?TIMEOUT, ?RATE]),
  {ok, #remove_old{timeout=?TIMEOUT, cleanup_rate=?RATE}}.

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
  remove_old_limiters(State#remove_old.timeout),
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