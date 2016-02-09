%%% coding: latin-1
%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @copyright (C) 2015, Tony Rogvall
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
	 trace_gl/4,
         debug/1, debug/2, debugf/1,
	 debug_gl/1, debug_gl/2, debugf_gl/1,
         info/1, info/2, infof/1,
	 info_gl/1, info_gl/2, infof_gl/1,
	 warning/1, warning/2, warningf/1,
	 warning_gl/1, warning_gl/2, warningf_gl/1,
         error/1, error/2, errorf/1,
	 error_gl/1, error_gl/2, errorf_gl/1,
	 trace_file/0,
	 trace_file/1,
	 clear/0]).

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
    lager:debug("arguments ignored.\n", []),
    ale_sup:start_link([]).

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
	    File::string() | console | default) -> 
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
	       File::string() | console | default) -> 
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
    

call({trace, _OnOrOff, _FilterList, _Level, _Client, File} = Trace) 
  when is_list(File); File =:= console; File =:=default ->
    %% Do we want to check file existence??
    gen_server:call(?SRV, Trace).

%%--------------------------------------------------------------------
%% @doc
%% Lists existing traces and clients.
%% @end
%%--------------------------------------------------------------------
-spec i() -> list(tuple()).

i() ->
    gen_server:call(?SRV, i).

%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, debug).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec debug(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

debug(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, debug).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, debug, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec debug(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom()),
	   File::string()) -> 
		   ok | {error, Error::term()}.

debug(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, debug, File).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, debug, default).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec debugf(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

debugf(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, debug, default).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, debug).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec debug_gl(ModuleOrPidOrList::atom() | 
				  string() | 
				  pid() | 
				  tuple() | 
				  list(atom())) -> 
		   ok | {error, Error::term()}.

debug_gl(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, debug, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, debug, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec debug_gl(ModuleOrPidOrList::atom() | 
				  string() | 
				  pid() | 
				  tuple() | 
				  list(atom()),
	       File::string()) -> 
		   ok | {error, Error::term()}.

debug_gl(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, debug, File, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, debug, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec debugf_gl(ModuleOrPidOrList::atom() | 
				  string() | 
				  pid() | 
				  tuple() | 
				  list(atom())) -> 
		   ok | {error, Error::term()}.

debugf_gl(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, debug, default, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, info).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec info(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

info(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, info).
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, info, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec info(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom()),
	       File::string()) -> 
		   ok | {error, Error::term()}.

info(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, info, File).
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, info, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec infof(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

infof(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, info, default).
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, info).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec info_gl(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

info_gl(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, info, gl).
                
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, info, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec info_gl(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom()),
	       File::string()) -> 
		   ok | {error, Error::term()}.

info_gl(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, info, File, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, info, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec infof_gl(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

infof_gl(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, info, default, gl).
                
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, warning).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec warning(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

warning(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, warning).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, warning, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec warning(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom()),
	      File::string()) -> 
		   ok | {error, Error::term()}.

warning(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, warning, File).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, warning, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec warningf(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

warningf(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, warning, default).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, warning).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec warning_gl(ModuleOrPidOrList::atom() | 
				    string() | 
				    pid() | 
				    tuple() | 
				    list(atom())) -> 
			ok | {error, Error::term()}.

warning_gl(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, warning, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, warning, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec warning_gl(ModuleOrPidOrList::atom() | 
				    string() | 
				    pid() | 
				    tuple() | 
				    list(atom()),
		 File::string()) -> 
			ok | {error, Error::term()}.

warning_gl(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, warning, File, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, warning, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec warningf_gl(ModuleOrPidOrList::atom() | 
				    string() | 
				    pid() | 
				    tuple() | 
				    list(atom())) -> 
			ok | {error, Error::term()}.

warningf_gl(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, warning, default, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, error).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec error(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

error(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, error).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, error, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec error(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom()),
	    File::string()) -> 
		   ok | {error, Error::term()}.

error(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, error, File).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace(on, X, error, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec errorf(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

errorf(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, error, default).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, error).
%% For details see {@link trace/3}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec error_gl(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

error_gl(ModulOrPidOrList) ->
    trace_i(ModulOrPidOrList, error, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, error, File).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec error_gl(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom()),
	       File::string()) -> 
		   ok | {error, Error::term()}.

error_gl(ModulOrPidOrList, File) ->
    tracef_i(ModulOrPidOrList, error, File, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Shortcut to trace_gl(on, X, error, default).
%% For details see {@link trace/4}.
%% Can be called with a list of modules.
%% @end
%%--------------------------------------------------------------------
-spec errorf_gl(ModuleOrPidOrList::atom() | 
				 string() | 
				 pid() | 
				 tuple() | 
				 list(atom())) -> 
		   ok | {error, Error::term()}.

errorf_gl(ModulOrPidOrList) ->
    tracef_i(ModulOrPidOrList, error, default, gl).
        
%%--------------------------------------------------------------------
%% @doc
%% Removes all traces for all clients.
%% @end
%%--------------------------------------------------------------------
-spec clear() -> ok.

clear() ->
    gen_server:call(?SRV, clear).


%%--------------------------------------------------------------------
%% @doc
%% Get default trace file.
%% Path relative lager log-root.
%% @end
%%--------------------------------------------------------------------
-spec trace_file() -> ok.

trace_file() ->
    gen_server:call(?SRV, trace_file).


%%--------------------------------------------------------------------
%% @doc
%% Set default trace file.
%% Path relative lager log-root.
%% @end
%%--------------------------------------------------------------------
-spec trace_file(File::string()) -> ok.

trace_file(File) ->
    gen_server:call(?SRV, {trace_file, File}).


%%--------------------------------------------------------------------
%% Test functions
%%--------------------------------------------------------------------
%% @private
start() ->
    app_ctrl([ale], start).

%% @private
stop() ->
    app_ctrl([ale, lager, goldrush],stop).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @private

app_ctrl([], _F) ->
    ok;
app_ctrl([App|Apps], F) ->
    lager:debug("~p: ~p\n", [F,App]),
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

%% @private
trace_i(Module, Level) ->
    trace_i(Module, Level, self).

trace_i(Module, Level, Type) 
  when is_atom(Module), is_atom(Level) ->
    trace_i([Module], Level, Type);
trace_i([], _Level, _Type) ->
    ok;
trace_i([Module | Rest], Level, self) 
  when is_atom(Module), is_atom(Level) ->
    trace(on, Module, Level),
    trace_i(Rest, Level, self);
trace_i([Module | Rest], Level, gl) 
  when is_atom(Module), is_atom(Level) ->
    trace_gl(on, Module, Level),
    trace_i(Rest, Level, gl);
trace_i([Filter | _Rest] = FilterList, Level, self) 
  when is_tuple(Filter), is_atom(Level) ->
    trace(on, FilterList, Level);
trace_i([Filter | _Rest] = FilterList, Level, gl) 
  when is_tuple(Filter), is_atom(Level) ->
    trace_gl(on, FilterList, Level).

tracef_i(Module, Level, File) ->
    tracef_i(Module, Level, File, self).

tracef_i(Module, Level, File, Type) 
  when is_atom(Module), is_atom(Level) ->
    tracef_i([Module], Level, File, Type);
tracef_i([], _Level, _File, _Type) ->
    ok;
tracef_i([Module | Rest], Level, File, self) 
  when is_atom(Module), is_atom(Level) ->
    trace(on, Module, Level, File),
    tracef_i(Rest, Level, File, self);
tracef_i([Module | Rest], Level, File, gl) 
  when is_atom(Module), is_atom(Level) ->
    trace_gl(on, Module, Level, File),
    tracef_i(Rest, Level, File, gl);
tracef_i([Filter | _Rest] = FilterList, Level, File, self) 
  when is_tuple(Filter), is_atom(Level) ->
    trace(on, FilterList, Level, File);
tracef_i([Filter | _Rest] = FilterList, Level, File, gl) 
  when is_tuple(Filter), is_atom(Level) ->
    trace_gl(on, FilterList, Level, File).

