%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 8. 4月 2020 11:20
%%%-------------------------------------------------------------------
-module(emqx_urap_connection).
-author("wwhai").

-behaviour(gen_server).

%% API
-export([start/1]).
-export([start_link/2, init/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(UDP_OPTS, [binary, {reuseaddr, true}]).

-record(eemqx_urap_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start(Port) ->
  Opts = [{udp_options, [binary, {reuseaddr, true}]}],
  MFA = {?MODULE, start_link, []},
  esockd:open_udp('urap:udp', Port, Opts, MFA).


start_link(Transport, Peer) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [Transport, Peer])}.

init(_T, _P) ->
  gen_server:enter_loop(?MODULE, [], #eemqx_urap_state{}).

init(_) -> ok.

%%
%% 消息接收处理
%%

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_info(Msg, State) ->
  io:format("MSG:~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.