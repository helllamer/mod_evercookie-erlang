-module(resource_evercookie_etag).

%% @doc inject etag into browser cache
%% Same as https://github.com/samyk/evercookie/blob/master/evercookie_etag.php

-export([
	init/1,
	service_available/2,
	content_types_provided/2,
	generate_etag/2,
	provide_content/2
]).

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").
-include("../include/evercookie.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.


service_available(ReqData, _DispatchArgs) ->
    Cookie = wrq:get_cookie_value(?COOKIE_ETAG, ReqData),
    {true, ReqData, Cookie}.


content_types_provided(ReqData, Cookie) ->
    CT = {"text/javascript", provide_content},
    {[CT], ReqData, Cookie}.


generate_etag(ReqData, undefined) ->
    Etag = evercookie_lib:get_etag(ReqData),
    {Etag, ReqData, Etag};
generate_etag(ReqData, Cookie) ->
    {Cookie, ReqData, Cookie}.


provide_content(ReqData, undefined=Cookie) ->
    {{halt, 204}, ReqData, Cookie};
provide_content(ReqData, Cookie) ->
    Output = evercookie_lib:output(Cookie),
    {Output, ReqData, Cookie}.

