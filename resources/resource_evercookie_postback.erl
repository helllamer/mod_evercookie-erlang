-module(resource_evercookie_postback).

-export([event/2]).


%% @doc receive evercookie from js and make broadcast if all ok
event({postback, cookie, _TriggerId, _TargetId}, Context) ->
    CookieValue = z_context:get_q("cookie", Context),
    try mod_evercookie:get_id(CookieValue, Context) of
	Id -> 
	    %% all ok - send async broadcast
	    z_notifier:notify({evercookie_id, Id}, Context),
	    Context

	catch _:_ ->
	    Context

    end.

