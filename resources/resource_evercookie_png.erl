-module(resource_evercookie_png).

%% @doc PNG-cookie setter.
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_png.php

-export([
	init/1,
	service_available/2,
	content_types_provided/2,
	last_modified/2,
	expires/2,
	provide_png/2
    ]).

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").
-include("../include/evercookie.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

%% @doc we don't have a cookie, user probably deleted it, force cache
service_available(ReqData, _DispatchArgs) ->
    Cookie = wrq:get_cookie_value(?COOKIE_PNG, ReqData),
    Result = case Cookie of
	undefined -> {halt, 304};
	_	  -> true
    end,
    {Result, ReqData, Cookie}.


%% @doc forge png output
content_types_provided(ReqData, Cookie) ->
    Result = [{"image/png", provide_png}],
    {Result, ReqData, Cookie}.

last_modified(ReqData, Cookie) ->
    ReqData1 = evercookie_lib:set_cache_control(ReqData),
    {?DATE_LAST_MODIFIED, ReqData1, Cookie}.

expires(ReqData, Cookie) ->
    {?DATE_EXPIRES, ReqData, Cookie}.


provide_png(ReqData, Cookie) -> 
    PhpScript = filename:join([z_utils:lib_dir(priv), "modules", "mod_evercookie", "php_src", "evercookie_png.php"]),
    Cmd	 = "php '" ++ PhpScript ++ "' '" ++ Cookie ++ "'",
    ?DEBUG(Cmd),
    {ok, Png} = dump_stdio(Cmd),
    %% and set caching headers
    {Png, ReqData, Cookie}.


dump_stdio(Cmd) ->
    Port = open_port({spawn, Cmd}, [use_stdio, exit_status, binary]),
    dump_stdio(Port, []).

dump_stdio(Port, Acc) ->
    receive
	{Port, {data, Bin}} -> 
	    dump_stdio(Port, [Bin | Acc]);

	{Port, {exit_status, _Exit}} ->
	    {ok, lists:reverse(Acc)}

	after 1000 ->
	    {error, timeout}

    end.
