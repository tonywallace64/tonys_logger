%%%-------------------------------------------------------------------
%%% @author tony <tony@faith>
%%% @copyright (C) 2016, tony
%%% @doc
%%% supervise writers
%%% @end
%%% Created : 24 Jun 2016 by tony <tony@faith>
%%%-------------------------------------------------------------------
-module(writer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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

    AChild = #{id => 'AName',
	       start => {'AModule', start_link, []},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => ['AModule']},

    {ok,WritersList} = application:get_env(tonys_logger,writers),
    io:format("writers: ~p~n",[WritersList]),
    ChildList = [AChild#{id => Id, start => {writer2, start_link, [{Id,File}]}} || {Id,File} <- WritersList],

    {ok, {SupFlags, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
