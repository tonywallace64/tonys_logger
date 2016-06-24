%%%-------------------------------------------------------------------
%%% @author tony <tony@faith>
%%% @copyright (C) 2016, tony
%%% @doc
%%% Receives and writes log requests
%%% Log requests may be calls or casts
%%% Calls are used for low volume high importance messages such as crashes
%%% Casts are used for high volume messages that can occasionally be dropped
%%%  such as recording web traffic or debugging messages
%%% @end
%%% Created : 24 Jun 2016 by tony <tony@faith>
%%%-------------------------------------------------------------------
-module(writer2).

-behaviour(gen_server).

%% API
-export([start_link/1,test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-include("tonys_logger.hrl").

-record(state,{hdl}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({Id,LogFile}) ->
    io:format("Starting writer~n"),
    gen_server:start_link({via,tonys_logger_svr,Id}, ?MODULE, [LogFile], []).
    
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
init(LogFile) ->
    FilePath = LogFile,
    %% default open for highest performance
    %% buffer can be flushed on an as required basis
    {ok,Hdl} = file:open(FilePath,[append,delayed_write]),
    State=#state{hdl=Hdl},
    {ok, State}.

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
handle_call({msg,Msg},_From,State) ->
    Hdl = State#state.hdl,
    MsgWriteState = write_msg(Msg,Hdl,sync),
    {reply,MsgWriteState,State};

handle_call({stop,Reason},_From,State) ->
    {stop,Reason,ok,State}.

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
handle_cast({msg,Msg}, State) ->
    Hdl = State#state.hdl,
    _MsgWriteState = write_msg(Msg,Hdl,no_sync),
    {noreply, State};
handle_cast({stop,Reason}, State) ->
    {stop,Reason,State}.


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
terminate(_Reason, State) ->
    Hdl = State#state.hdl,
    ok=close(Hdl).

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


%%% write_msg - crash if file errors occur
write_msg(Msg,Hdl,sync) ->
    ok = file:write(Hdl,Msg),
    ok = file:datasync(Hdl);
write_msg(Msg,Hdl,no_sync) ->
    ok = file:write(Hdl,Msg).

close(Hdl) ->
    close(file:close(Hdl),Hdl).
close(ok,_Hdl) ->
    ok; 
close({error,_},Hdl)->
    %% try again, delayed write with error need to do this twice
    %% see documentation.  Hopefully this time ok.
    ok=file:close(Hdl).

%% ==============================================
%% Self test
%% ==============================================

format(Msg) ->
    io_lib:format("~s~n",[Msg]).
test() ->
    file:delete("test.log"),
    %Writer = spawn(?MODULE,init,["test.log"]),
    {ok,Pid} = start_link("test.log"),
    ok=gen_server:call(Pid,{msg,format("message 1")}),
    ok=gen_server:cast(Pid,{msg,format("message 2")}),
    %% use shutdown call instead of cast to prevent race
    %% condition with reading the log file.
    ok=gen_server:call(Pid,{stop,shutdown}),
    {ok,<<"message 1/nmessage 2/n">>} = file:readfile("test.log").
