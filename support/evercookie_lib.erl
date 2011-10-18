-module(evercookie_lib).

%% Shared routines.

-compile(export_all).
-include("../include/evercookie.hrl").

set_cache_control(ReqData) ->
    wrq:set_resp_header("Cache-Control", ?CACHE_CONTROL, ReqData).

get_etag(ReqData) ->
    wrq:get_req_header("If-None-Match", ReqData).

set_etag(Value, ReqData) ->
    wrq:set_resp_header("Etag", Value, ReqData).

output(Text) ->
    [<<"//">>, Text].
