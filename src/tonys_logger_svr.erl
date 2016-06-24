%%%-------------------------------------------------------------------
%%% @author tony <tony@faith>
%%% @copyright (C) 2016, tony
%%% @doc
%%% Maintains ets table for logger
%%% This table allows logging processes to write directly to writers
%%% avoiding gen_server bottlenecks.
%%% @end
%%% Created : 24 Jun 2016 by tony <tony@faith>
%%%-------------------------------------------------------------------
-module(tonys_logger_svr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% name interface to receive gen_server name registrations
-export([register_name/2,unregister_name/1,whereis_name/1,send/2]).

-define(SERVER, ?MODULE).
-define(LI, tonys_logger_info).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
register_name(Name,Pid) ->
    gen_server:call(?SERVER,{register_name,Name,Pid}).

send(Name,Msg) ->
    Pid = whereis_name(Name),
    Pid ! Msg,
    Pid.

unregister_name(Name) ->
    gen_server:call(?SERVER,{deregister_name,Name}).

whereis_name(Name) ->
    gen_server:call(?SERVER,{whereis,Name}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    
    ?LI = ets:new(?LI,[set,protected,named_table,{read_concurrency,true}]),
    %?LI = ets:new(?LI,[set,named_table]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register_name,Name,Pid}, _From, State) ->
    true = ets:insert(?LI,{Name,Pid}),
    Reply = yes,
    {reply, Reply, State};
handle_call({deregister_name,Name},_,State) ->
    Reply = maybe_deregister(Name),
    {reply, Reply, State};
handle_call({whereis,Name},_,State) ->
    Reply = whereis2(ets:lookup(?LI,Name)),
    {reply, Reply, State}.
    


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_deregister(Name) ->
    ets:delete(?LI,Name).
 
whereis2([{_Name,Pid}]) -> 
    Pid;
whereis2(_) ->
    undefined.
    
    
