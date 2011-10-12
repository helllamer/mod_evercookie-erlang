-module(resource_evercookie_png).

%% @doc PNG-cookie setter.
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_png.php

-export([
	init/1,
	service_available/2,
	content_types_provided/2,
	provide_png/2
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


%% @doc only png output
content_types_provided(ReqData, Cookie) ->
    Result = [{"image/png", provide_png}],
    {Result, ReqData, Cookie}.


provide_png(ReqData, Cookie) -> 
    PhpScript = filename:join([z_utils:lib_dir(priv), "modules", "mod_evercookie", "php_src", "evercookie_png.php"]),
    Port = open_port({spawn, "php '" ++ PhpScript ++ "' '" ++ Cookie ++ "'"}, [stream, eof, exit_status, binary]),
    Png  = receive {Port, {data, Bin}} -> Bin	       after 500 -> timeout end,
    _End = receive {Port, {exit_status, Exit}} -> Exit after 100 -> timeout end,
    Hdrs = [
	{"Last-Modified", httpd_util:rfc1123_date(?DATE_LAST_MODIFIED)},
	{"Expires",	  httpd_util:rfc1123_date(?DATE_EXPIRES)},
	{"Cache-Control", "private, max-age=630720000"}
    ],
    %% and set caching headers
    ReqData1 = wrq:merge_resp_headers(Hdrs, ReqData),
    {Png, ReqData1, Cookie}.

