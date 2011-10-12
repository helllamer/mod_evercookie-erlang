-module(resource_evercookie_etag).

%% @doc inject etag into browser cache
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_etag.php

-export([ init/1, to_html/2 ]).

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").
-include("../include/evercookie.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.


%% if cookie is set - ensure the etag. Result is cookie/etag body
to_html(ReqData, DispatchArgs) ->
    {Result, ResultReqData} = case wrq:get_cookie_value(?COOKIE_ETAG, ReqData) of
	%% we don't have a cookie, so we're not setting it
	undefined ->
	    Etag    = wrq:get_req_header("If-None-Match", ReqData),
	    Result0 = z_utils:coalesce([Etag, ""]),
	    {Result0, ReqData};

	%% set our etag, return the cookie value
	Cookie ->
	    ReqData1 = wrq:set_resp_header("Etag", Cookie, ReqData),
	    {Cookie, ReqData1}

    end,
    {Result, ResultReqData, DispatchArgs}.
