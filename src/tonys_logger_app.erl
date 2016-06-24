%%%-------------------------------------------------------------------
%%% @author tony <tony@faith>
%%% @copyright (C) 2016, tony
%%% @doc
%%%
%%% @end
%%% Created : 24 Jun 2016 by tony <tony@faith>
%%%-------------------------------------------------------------------
-module(tonys_logger_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([start_link/0]).
%% API

-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    io:format("starting application~n"),
    case start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
%%%===================================================================
%%% API functions
%%%===================================================================



%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild1 = #{id => tonys_logger_svr,
	       start => {tonys_logger_svr, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [tonys_logger_svr]},

    AChild2 = #{id => writer_sup,
	       start => {writer_sup, start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => [writer_sup]},

    {ok, {SupFlags, [AChild1,AChild2]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

