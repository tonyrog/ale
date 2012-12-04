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
%%%    A lager extension.
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

%% Start/Stop traces
-export([trace/3,
	 trace_gl/3,
	 trace/4,
	 trace_gl/4]).

%% Info requests
-export([i/0]).

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
     Traces = case application:get_env(init_traces) of
	       undefined -> [];
	       {ok, T} -> T
	   end,
    Args = [{options, Opts},{init_traces, Traces}],
    ale_sup:start_link(Args).

%%--------------------------------------------------------------------
%% @doc
%% Stops the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State::term()) -> ok | {error, Error::term()}.

stop(_State) ->
    ok.

-type log_level() :: debug | 
		     info | 
		     notice | 
		     warning | 
		     error | 
		     critical | 
		     alert | 
		     emergency.

%%--------------------------------------------------------------------
%% @doc
%% Controls tracing.
%% For details see lager documentation.
%% @end
%%--------------------------------------------------------------------
-spec trace(OnOrOff:: on | off, 
	    ModuleOrPidOrFilter::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(tuple()), 
	    Level::log_level()) -> 
		   ok | {error, Error::term()}.

trace(OnOrOff, ModulOrPidOrFilter, Level) ->
    trace(OnOrOff, ModulOrPidOrFilter, Level, console).
        
%%--------------------------------------------------------------------
%% @doc
%% Controls tracing.
%% For details see lager documentation.
%% @end
%%--------------------------------------------------------------------
-spec trace(OnOrOff:: on | off, 
	    ModuleOrPidOrFilter::atom() | 
				 pid() | 
				 tuple() | 
				 list(tuple()), 
	    Level::log_level(),
	    File::string() | console) -> 
		   ok | {error, Error::term()}.

trace(OnOrOff, Module, Level, File) 
  when is_atom(OnOrOff), is_atom(Module), is_atom(Level) ->
    call({trace, OnOrOff, [{module, Module}], Level, self(), File});
trace(OnOrOff, Pid, Level, File) 
  when is_atom(OnOrOff), is_pid(Pid), is_atom(Level) ->
    call({trace, OnOrOff, [{pid, pid_to_list(Pid)}], Level, self(), File});
trace(OnOrOff, Filter, Level, File) 
  when is_atom(OnOrOff), is_tuple(Filter),is_atom(Level) ->
    call({trace, OnOrOff, [Filter], Level, self(), File});
trace(OnOrOff, FilterList, Level, File) 
  when is_atom(OnOrOff), is_list(FilterList),is_atom(Level) ->
    call({trace, OnOrOff, FilterList, Level, self(), File}).
    
%%--------------------------------------------------------------------
%% @doc
%% Controls tracing.
%% This variant uses the groupleader() instead of self() to monitor
%% client. Suitable for calls from an erlang shell.
%% For details see lager documentation.
%% @end
%%--------------------------------------------------------------------
-spec trace_gl(OnOrOff:: on | off, 
	      ModuleOrPidOrFilter::atom() | 
				   string() | 
				   pid() | 
				   tuple() | 
				   list(tuple()), 
	      Level::log_level()) -> 
		     ok | {error, Error::term()}.

trace_gl(OnOrOff, Module, Level) ->
  trace_gl(OnOrOff, Module, Level, console).

%%--------------------------------------------------------------------
%% @doc
%% Controls tracing.
%% This variant uses the groupleader() instead of self() to monitor
%% client. Suitable for calls from an erlang shell.
%% For details see lager documentation.
%% @end
%%--------------------------------------------------------------------
-spec trace_gl(OnOrOff:: on | off, 
	       ModuleOrPidOrFilter::atom() | 
				    pid() | 
				    tuple() | 
				    list(tuple()), 
	       Level::log_level(),
	       File::string() | console) -> 
		     ok | {error, Error::term()}.

trace_gl(OnOrOff, Module, Level, File) 
  when is_atom(OnOrOff), is_atom(Module), is_atom(Level) ->
    call({trace, OnOrOff, [{module, Module}], Level,  group_leader(), File});
trace_gl(OnOrOff, Pid, Level, File) 
  when is_atom(OnOrOff), is_pid(Pid), is_atom(Level) ->
    call({trace, OnOrOff, [{pid, pid_to_list(Pid)}], Level, 
	  group_leader(), File});
trace_gl(OnOrOff, Filter, Level, File) 
  when is_atom(OnOrOff), is_tuple(Filter), is_atom(Level) ->
    call({trace, OnOrOff, [Filter], Level,  group_leader(), File});
trace_gl(OnOrOff, FilterList, Level, File) 
  when is_atom(OnOrOff), is_list(FilterList), is_atom(Level) ->
    call({trace, OnOrOff, FilterList, Level, group_leader(), File}).
    

call({trace, _OnOrOff, _FilterList, _Level, _Client, console} = Trace) ->
    gen_server:call(?SRV, Trace);
call({trace, _OnOrOff, _FilterList, _Level, _Client, File} = Trace) 
  when is_list(File) ->
    %% Do we want this check ??
    case filelib:is_regular(File) of
	true ->
	    gen_server:call(?SRV, Trace);
	false ->
	    {error, non_existing_file}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Lists existing traces and clients.
%% @end
%%--------------------------------------------------------------------
-spec i() -> list(tuple()).

i() ->
    gen_server:call(?SRV, i).

%%--------------------------------------------------------------------
%% Test functions
%%--------------------------------------------------------------------
%% @private
start() ->
    app_ctrl([kernel, stdlib, compiler, syntax_tools, sasl, lager, ale],start).

stop() ->
    app_ctrl([ale, lager],stop).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @private
app_ctrl([], _F) ->
    ok;
app_ctrl([App|Apps], F) ->
    error_logger:info_msg("~p: ~p\n", [F,App]),
    case application:F(App) of
	{error,{not_started,App1}} ->
	    case F of
		start ->
		    app_ctrl([App1,App|Apps], F);
		stop ->
		    app_ctrl(Apps, F)
	    end;
	{error,{already_started,App}} ->
	    app_ctrl(Apps, F);
	ok ->
	    app_ctrl(Apps, F);
	Error ->
	    Error
    end.


