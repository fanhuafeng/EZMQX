%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 3月 2020 11:20
%%%-------------------------------------------------------------------
-module(emqx_trap_connection).
-author("admin").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(emqx_trap_connection_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Transport, Socket) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Socket]])}.
init([Transport, Socket]) ->
    case Transport:wait(Socket) of
        {ok, NewSocket} ->
            {ok, {IP, Port}} = Transport:peername(NewSocket),
            io:format("New socket connected: Ip is :~p and port is ~p ~n", [IP, Port]),
            Transport:setopts(Socket, [{active, once}]),
            %% 先挂起来等认证,防止恶意连接
            %% erlang:send_after(3000, self(), wait_for_auth),
            %% 进入下一次循环
            gen_server:enter_loop(?MODULE, [], #emqx_trap_connection_state{});
        {error, Reason} ->
            {stop, Reason}
    end.
%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #emqx_trap_connection_state{}) ->
    {reply, Reply :: term(), NewState :: #emqx_trap_connection_state{}} |
    {reply, Reply :: term(), NewState :: #emqx_trap_connection_state{}, timeout() | hibernate} |
    {noreply, NewState :: #emqx_trap_connection_state{}} |
    {noreply, NewState :: #emqx_trap_connection_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #emqx_trap_connection_state{}} |
    {stop, Reason :: term(), NewState :: #emqx_trap_connection_state{}}).
handle_call(_Request, _From, State = #emqx_trap_connection_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #emqx_trap_connection_state{}) ->
    {noreply, NewState :: #emqx_trap_connection_state{}} |
    {noreply, NewState :: #emqx_trap_connection_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #emqx_trap_connection_state{}}).
handle_cast(_Request, State = #emqx_trap_connection_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #emqx_trap_connection_state{}) ->
    {noreply, NewState :: #emqx_trap_connection_state{}} |
    {noreply, NewState :: #emqx_trap_connection_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #emqx_trap_connection_state{}}).
handle_info(_Info, State = #emqx_trap_connection_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #emqx_trap_connection_state{}) -> term()).
terminate(_Reason, _State = #emqx_trap_connection_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #emqx_trap_connection_state{},
    Extra :: term()) ->
    {ok, NewState :: #emqx_trap_connection_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #emqx_trap_connection_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
