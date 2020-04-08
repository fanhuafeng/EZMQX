%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 3月 2020 11:20
%%%-------------------------------------------------------------------
-module(emqx_trap_connection).
-author("wwhai").

-behaviour(gen_server).

%% API
-export([start_link/3, init/1]).

%% gen_server callbacks
-export([init/4, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(emqx_trap_connection_state, {transport, socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Transport, Socket, Options) ->
    Args = [self(), Transport, Socket, Options],
    CPid = proc_lib:spawn_link(?MODULE, init, Args),
    {ok, CPid}.
%%
%% OTP的默认回调,留空
init(_) ->
    ok.
init(_Parent, Transport, RawSocket, _Options) ->
    case Transport:wait(RawSocket) of
        {ok, NewSocket} ->
            {ok, {IP, Port}} = Transport:peername(NewSocket),
            io:format("New socket connected: Ip is :~p and port is ~p ~n", [IP, Port]),
            Transport:setopts(RawSocket, [{active, once}]),

            gen_server:enter_loop(?MODULE, [], #emqx_trap_connection_state{transport = Transport, socket = RawSocket});
        {error, Reason} ->
            ok = Transport:fast_close(RawSocket),
            exit_on_sock_error(Reason),
            {stop, Reason}
    end.
exit_on_sock_error(Reason) when Reason =:= einval;
    Reason =:= enotconn;
    Reason =:= closed ->
    erlang:exit(normal);
exit_on_sock_error(timeout) ->
    erlang:exit({shutdown, ssl_upgrade_timeout});
exit_on_sock_error(Reason) ->
    erlang:exit({shutdown, Reason}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%%
%% 消息接收处理
%%

handle_info({tcp, RemoteSocket, BinData}, #emqx_trap_connection_state{transport = Transport} = State) ->
    Transport:setopts(RemoteSocket, [{active, once}]),
    io:format("Rawdata From: ~p ~n", [RemoteSocket]),
    binpp:pprint(BinData),
    {noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
    io:format("Error: ~p  from: ~p~n", [Reason, Socket]),
    {stop, normal, State};

handle_info({tcp_closed, Socket}, State) ->
    io:format("Socket:~p closed. ~n", [Socket]),

    {stop, normal, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

