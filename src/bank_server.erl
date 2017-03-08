%%%-------------------------------------------------------------------
%%% @author nekrasov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(bank_server).
-author("nekrasov").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([getCurrency/0,
  getBalance/2,
  putMoney/3,
  getMoney/3,
  sendMoney/4,
  giveMoney/3,
  getLimitMoney/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

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
  bank:main(),
  {ok, #state{}}.

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
handle_call(get_currency, _From, State) ->
  Reply = bank:getCurrency(),
  {reply, Reply, State};
handle_call({get_balance, Deposit, Pin}, _From, State) ->
  Reply = bank:getBalance(Deposit, Pin),
  {reply, Reply, State};
handle_call({put_money, Deposit, Pin, Sum}, _From, State) ->
  Reply = bank:putMoney(Deposit, Pin, Sum),
  {reply, Reply, State};
handle_call({get_money, Deposit, Pin, Sum}, _From, State) ->
  Reply = bank:getMoney(Deposit, Pin, Sum),
  {reply, Reply, State};
handle_call({send_money, Deposit, Pin, DepositTo, Sum}, _From, State) ->
  Reply = bank:sendMoney(Deposit, Pin, DepositTo, Sum),
  {reply, Reply, State};
handle_call({give_money, Deposit, Pin, SumTo}, _From, State) ->
  Reply = bank:giveMoney(Deposit, Pin, SumTo),
  {reply, Reply, State};
handle_call({get_limit}, _From, State) ->
  Reply = bank:getLimit(),
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

getCurrency() ->
  gen_server:call({global, ?MODULE}, get_currency).

getBalance(Deposit, Pin) ->
  gen_server:call({global, ?MODULE}, {get_balance, Deposit, Pin}).

putMoney(Deposit, Pin, Sum) ->
  gen_server:call({global, ?MODULE}, {put_money, Deposit, Pin, Sum}).

getMoney(Deposit, Pin, Sum) ->
  gen_server:call({global, ?MODULE}, {get_money, Deposit, Pin, Sum}).

sendMoney(Deposit, Pin, DepositTo, Sum) ->
  gen_server:call({global, ?MODULE}, {send_money, Deposit, Pin, DepositTo, Sum}).

giveMoney(Deposit, Pin, SumTo) ->
  gen_server:call({global, ?MODULE}, {give_money, Deposit, Pin, SumTo}).

getLimitMoney()->
  gen_server:call({global, ?MODULE}, {get_limit}).
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
handle_info(_Info, State) ->
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
