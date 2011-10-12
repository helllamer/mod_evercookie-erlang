-module(resource_evercookie_cache).

%% force cache, echo cookie
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_cache.php

-export([
	init/1,
	service_available/2,
	to_html/2
    ]).

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").
-include("../include/evercookie.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.


%% @doc we don't have a cookie, user probably deleted it, force cache
service_available(ReqData, _DispatchArgs) ->
    Cookie = wrq:get_cookie_value(?COOKIE, ReqData),
    Result = case Cookie of
	undefined -> {halt, 304};
	_	  -> true
    end,
    {Result, ReqData, Cookie}.


%% set resp.headers, echo evercookie as responce.
to_html(ReqData, Cookie) ->
    Headers = [
	{"Last-Modified",   httpd_util:rfc1123_date(?DATE_LAST_MODIFIED)},
	{"Expires",	    httpd_util:rfc1123_date(?DATE_EXPIRES)},
	{"Cache-Control",   "private, max-age=630720000"}
    ],
    ReqData1 = wrq:merge_resp_headers(Headers, ReqData),
    {Cookie, ReqData1, Cookie}.

