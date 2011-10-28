-module(resource_evercookie_postback).

%% Handle evercookie.js sniffing.

-export([event/2]).

-include("zotonic.hrl").

%% @doc receive evercookie from js and make broadcast notification if all ok
event({postback, cookie, _TriggerId, _TargetId}, Context) ->
    CookieValue = z_context:get_q("cookie", Context),
    UserId = z_acl:user(Context),
    case mod_evercookie:get_id(CookieValue, Context) of
	{ok, Id} -> id_ok(UserId, Id, Context);
	_	 -> id_fail(UserId, Context)
    end.


%% @doc Cookie parsed ok: push into db, display nothing by now.
id_ok(UserId, Id, Context) ->
    m_evercookie:insert(UserId, Id, Context),
    Context.


%% @doc received bad or empty cookie. Let's regenerate and store new id (if user not anonymous).
id_fail(undefined, Context) ->
    Context;
id_fail(UserId, Context) ->
    CookieValue = mod_evercookie:new(Context),
    Id = mod_evercookie:get_id(CookieValue, Context),
    id_ok(UserId, Id, Context),
    %% and render new cookie to the client
    z_render:wire({set_ec, [{cookie, CookieValue}]}, Context).
