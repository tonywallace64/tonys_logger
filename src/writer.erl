-module (writer).

-export([init/1,terminate/2,log_message/2,log_message/3,test/0]).

-record(state,{hdl}).

init(FilePath) ->
    %% default open for highest performance
    %% buffer can be flushed on an as required basis
    {ok,Hdl} = file:open(FilePath,[append,delayed_write]),
    State=#state{hdl=Hdl},
    loop(State).

terminate(Writer,Reason) ->
    Writer ! {exit,Reason}.

log_message(Ref,Writer,Msg) ->
    Writer ! {msg,{self(),Ref},Msg}.

log_message(Writer,Msg) ->
    Writer ! {msg,self(),Msg}.

loop(State) ->
    Hdl = State#state.hdl,
    receive
	{msg,From,Msg} ->
	    write_msg(Msg,Hdl),
	    acknowledge(From),
	    loop(State);
	{exit,Reason} ->
	    %write_msg({exit,Reason},Hdl),
	    ok=close(Hdl);
	X -> throw({unexpected_message,X})
    end.

write_msg(Msg,Hdl) ->
    ok = file:write(Hdl,Msg).

acknowledge({Pid,Ref}) ->
    Pid ! {ok,Ref};
acknowledge(_) ->
    ok.

close(Hdl) ->
    close(file:close(Hdl),Hdl).
close(ok,_Hdl) ->
    ok; 
close({error,_},Hdl)->
    %% try again, delayed write with error need to do this twice
    %% see documentation.  Hopefully this time ok.
    file:close(Hdl).


format(Msg) ->
    io_lib:format("~s~n",[Msg]).
test() ->
    file:delete("test.log"),
    Writer = spawn(?MODULE,init,["test.log"]),
    log_message(Writer,format("message 1")),
    Ref = make_ref(),
    log_message(Ref,Writer,format("message 2")),
    io:format("Wait confirmation~n"),
    receive
	{ok,Ref} ->
	    io:format("confirmation received~n")
    after 10000 ->
	    io:format("confirmation not received, timeout~n")
    end,
    terminate(Writer,normal).
    
