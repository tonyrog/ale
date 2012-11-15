%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author  Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    ale application.
%%%    A lager extension..
%%%
%%% Created : 2012 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------

-module(ale).

-behaviour(application).

%% Application callbacks
-export([start/2, 
	 stop/1]).

%% Shortcut API
-export([start/0,
	 stop/0]).

%% Application API
-export([trace/3]).
-export([trace_gl/3]).

-define(SRV, ale_srv).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the application.<br/>
%% Arguments are ignored, instead the options for the application server are 
%% retreived from the application environment (sys.config).
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType:: normal | 
			{takeover, Node::atom()} | 
			{failover, Node::atom()}, 
	    StartArgs::term()) -> 
		   {ok, Pid::pid()} |
		   {ok, Pid::pid(), State::term()} |
		   {error, Reason::term()}.

start(_StartType, _StartArgs) ->
    error_logger:info_msg("~p: start: arguments ignored.\n", [?MODULE]),
    Opts = case application:get_env(options) of
	       undefined -> [];
	       {ok, O} -> O
	   end,
    Args = [{options, Opts}],
    ale_sup:start_link(Args).

%%--------------------------------------------------------------------
%% @doc
%% Stops the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State::term()) -> ok | {error, Error::term()}.

stop(_State) ->
    exit(stopped).

%%--------------------------------------------------------------------
%% @doc
%% Controls tracing.
%% For details see lager documentation.
%% @end
%%--------------------------------------------------------------------
-type log_level() :: debug | 
		     info | 
		     notice | 
		     warning | 
		     error | 
		     critical | 
		     alert | 
		     emergency.

-spec trace(OnOrOff:: on | off, 
	    ModuleOrPid::atom() | string() | pid(), 
	    Level::log_level()) -> 
		   ok | {error, Error::term()}.

trace(OnOrOff, Module, Level) 
  when is_atom(Module), is_atom(Level), is_atom(OnOrOff) ->
    gen_server:call(?SRV, {trace, OnOrOff, [{module, Module}], Level, self()});
trace(OnOrOff, Module, Level) 
  when is_list(Module), is_atom(Level), is_atom(OnOrOff)->
    trace(OnOrOff, list_to_atom(Module), Level);
trace(OnOrOff, Pid, Level) 
  when is_pid(Pid), is_atom(Level), is_atom(OnOrOff) ->
    gen_server:call(?SRV, {trace, OnOrOff, [{pid, pid_to_list(Pid)}], Level, self()}).
    
%%--------------------------------------------------------------------
%% @doc
%% Controls tracing.
%% This variant uses the groupleader() instead of self() to monitor
%% client. Suitable for calls from an erlang shell.
%% For details see lager documentation.
%% @end
%%--------------------------------------------------------------------
-spec trace_gl(OnOrOff:: on | off, 
	      ModuleOrPid::atom() | string() | pid(), 
	      Level::log_level()) -> 
		     ok | {error, Error::term()}.

trace_gl(OnOrOff, Module, Level) 
  when is_atom(Module), is_atom(Level), is_atom(OnOrOff) ->
    gen_server:call(?SRV, {trace, OnOrOff, [{module, Module}], Level, 
			      group_leader()});
trace_gl(OnOrOff, Module, Level) 
  when is_list(Module), is_atom(Level), is_atom(OnOrOff)->
    trace_gl(OnOrOff, list_to_atom(Module), Level);
trace_gl(OnOrOff, Pid, Level) 
  when is_pid(Pid), is_atom(Level), is_atom(OnOrOff) ->
    gen_server:call(?SRV, {trace, OnOrOff, [{pid, pid_to_list(Pid)}], Level, 
			      group_leader()}).
    
%% @private
start() ->
    call([sasl, lager, ale],start).

stop() ->
    call([ale, lager],stop).

call([], _F) ->
    ok;
call([App|Apps], F) ->
    error_logger:info_msg("~p: ~p\n", [F,App]),
    case application:F(App) of
	{error,{not_started,App1}} ->
	    call([App1,App|Apps], F);
	{error,{already_started,App}} ->
	    call(Apps, F);
	ok ->
	    call(Apps, F);
	Error ->
	    Error
    end.


