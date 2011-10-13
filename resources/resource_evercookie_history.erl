-module(resource_evercookie_history).

%% @doc Web history storage knocker backend. Any request will be responced with "204 No Content".
%% See http://samy.pl/evercookie/ -> FAQ -> "How does the Web History storage work"

-export([ init/1, service_available/2 ]).

-include_lib("webmachine_resource.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) ->
    {{halt, 204}, ReqData, DispatchArgs}.
