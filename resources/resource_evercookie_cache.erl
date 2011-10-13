-module(resource_evercookie_cache).

%% force cache, echo cookie
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_cache.php

-export([
	init/1,
	service_available/2,
	content_types_provided/2,
	last_modified/2,
	expires/2,
	provide_content/2
    ]).

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").
-include("../include/evercookie.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.


%% @doc we don't have a cookie, user probably deleted it, force cache
service_available(ReqData, _DispatchArgs) ->
    Cookie = wrq:get_cookie_value(?COOKIE_CACHE, ReqData),
    ?DEBUG({?COOKIE_CACHE, Cookie}),
    Result = case Cookie of
	undefined -> {halt, 304};
	_	  -> true
    end,
    {Result, ReqData, Cookie}.

content_types_provided(ReqData, DispatchArgs) ->
    CT = {"image/png", provide_content},
    {[CT], ReqData, DispatchArgs}.

last_modified(ReqData, Cookie) ->
    ReqData1 = evercookie_lib:set_cache_control(ReqData),
    {?DATE_LAST_MODIFIED, ReqData1, Cookie}.

expires(ReqData, Cookie) ->
    {?DATE_EXPIRES, ReqData, Cookie}.

%% set resp.headers, echo evercookie as responce.
provide_content(ReqData, Cookie) ->
    {Cookie, ReqData, Cookie}.

