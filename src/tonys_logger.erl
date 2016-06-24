-module(tonys_logger).

-export([log/2]).

log(Level_ID,Msg) ->
    {ok,AllLevels} = application:get_env(tonys_logger,levels),
    [{Level_ID,Writer,Sync}] = lists:filter( fun(X)->element(1,X) == Level_ID end, AllLevels),
    [{Writer,Pid}] = ets:lookup(tonys_logger_info,Writer),
    send_msg(Pid,Msg,Sync).

send_msg(Pid,Msg,sync) ->
    ok=gen_server:call(Pid,{msg,Msg});
send_msg(Pid,Msg,nosync) ->
    gen_server:cast(Pid,{msg,Msg}).
    
    
