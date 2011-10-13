-module(resource_evercookie_etag).

%% @doc inject etag into browser cache
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_etag.php

-export([
	init/1,
	content_types_provided/2,
	provide_content/2
]).

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").
-include("../include/evercookie.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

content_types_provided(ReqData, DispatchArgs) ->
    CT = {"image/png", provide_content},
    {[CT], ReqData, DispatchArgs}.

%% if cookie is set - ensure the etag. Result is cookie/etag body
provide_content(ReqData, DispatchArgs) ->
    Cookie = wrq:get_cookie_value(?COOKIE_ETAG, ReqData),
    ?DEBUG({?COOKIE_ETAG, Cookie}),
    {Result, ResultReqData} = case Cookie of
	%% we don't have a cookie, so we're not setting it
	undefined ->
	    Etag    = evercookie_lib:get_etag(ReqData),
	    ?DEBUG({etag, Etag}),
	    Etag1   = z_utils:coalesce([Etag, ""]),
	    {Etag1, ReqData};

	%% set our etag, return the cookie value
	_ ->
	    ReqData1 = evercookie_lib:set_etag(Cookie, ReqData),
	    {Cookie, ReqData1}

    end,
    {Result, ResultReqData, DispatchArgs}.



