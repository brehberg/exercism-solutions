-module(bank_account).

-behaviour(gen_server).

-export([
    create/0,
    balance/1,
    close/1,
    charge/2,
    deposit/2,
    withdraw/2
]).

-export([handle_call/3, handle_cast/2, init/1]).

create() ->
    {ok, Pid} = gen_server:start_link(?MODULE, 0, []),
    Pid.

balance(Pid) -> gen_server:call(Pid, balance).

close(Pid) -> gen_server:call(Pid, close).

deposit(Pid, Amount) -> gen_server:call(Pid, {deposit, Amount}).

withdraw(Pid, Amount) -> gen_server:call(Pid, {withdraw, Amount}).

charge(Pid, Amount) -> gen_server:call(Pid, {charge, Amount}).

init(Balance) -> {ok, {open, Balance}}.

handle(_, State = {closed, _}) ->
    {reply, {error, account_closed}, State};
handle(balance, State = {_, Amount}) ->
    {reply, Amount, State};
handle(close, {open, Balance}) ->
    {reply, Balance, {closed, Balance}};
handle({deposit, Amount}, State = {open, _}) when Amount < 0 ->
    {reply, {error, bad_arg}, State};
handle({deposit, Amount}, {open, Balance}) ->
    {reply, ok, {open, Balance + Amount}};
% withdraw for more than balance should return available
handle({withdraw, Amount}, State = {open, _}) when Amount < 0 ->
    {reply, 0, State};
handle({withdraw, Amount}, {open, Balance}) when Amount > Balance ->
    {reply, Balance, {open, 0}};
handle({withdraw, Amount}, {open, Balance}) ->
    {reply, Amount, {open, Balance - Amount}};
% charge for more than balance should return nothing
handle({charge, Amount}, State = {open, _}) when Amount < 0 ->
    {reply, 0, State};
handle({charge, Amount}, State = {open, Balance}) when Amount > Balance ->
    {reply, 0, State};
handle({charge, Amount}, {open, Balance}) ->
    {reply, Amount, {open, Balance - Amount}}.

handle_call(Request, _From, State) ->
    handle(Request, State).

handle_cast(Request, State) ->
    case handle(Request, State) of
        {reply, _, NewState} -> {noreply, NewState};
        Answer -> Answer
    end.
