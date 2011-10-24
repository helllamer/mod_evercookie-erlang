-module(resource_evercookie_postback).

%% Handle evercookie.js sniffing.
%% Successive reports are broadcasted via z_notifier: {evercookie_id, Id} where Id is string like z_ids:id() result.

-export([event/2]).

-include("zotonic.hrl").

%% @doc receive evercookie from js and make broadcast notification if all ok
event({postback, cookie, _TriggerId, _TargetId}, Context) ->
    CookieValue = z_context:get_q("cookie", Context),
    case mod_evercookie:get_id(CookieValue, Context) of
	{ok, Id} ->
	    UserId = z_acl:user(Context),
	    m_evercookie:insert(UserId, Id, Context);

	X ->
	    ?DEBUG({badarg, X, CookieValue}),
	    ok
    end,
    Context.

