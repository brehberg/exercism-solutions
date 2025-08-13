-module(circular_buffer).

-behaviour(gen_server).

-export([
    create/1,
    read/1,
    size/1,
    write/2,
    write_attempt/2
]).

-export([handle_call/3, handle_cast/2, init/1]).

create(Size) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Size, []),
    Pid.

read(Pid) -> gen_server:call(Pid, read).

size(Pid) -> gen_server:call(Pid, size).

write(Pid, Item) -> gen_server:cast(Pid, {write, Item}).

write_attempt(Pid, Item) -> gen_server:call(Pid, {write_attempt, Item}).

init(Size) -> {ok, {Size, []}}.

handle(read, Buffer = {_, []}) ->
    {reply, {error, empty}, Buffer};
handle(read, {Size, Items}) ->
    {reply, {ok, lists:last(Items)}, {Size, lists:droplast(Items)}};
handle(size, Buffer = {Size, _}) ->
    {reply, {ok, Size}, Buffer};
handle({write, Item}, {Size, Items}) when
    Size == length(Items)
->
    {reply, {error, ok}, {Size, [Item | lists:droplast(Items)]}};
handle({write, Item}, {Size, Items}) ->
    {reply, ok, {Size, [Item | Items]}};
handle({write_attempt, _}, Buffer = {Size, Items}) when
    Size == length(Items)
->
    {reply, {error, full}, Buffer};
handle({write_attempt, Item}, {Size, Items}) ->
    {reply, ok, {Size, [Item | Items]}}.

handle_call(Request, _From, State) ->
    handle(Request, State).

handle_cast(Request, State) ->
    case handle(Request, State) of
        {reply, _, NewState} -> {noreply, NewState};
        Answer -> Answer
    end.
