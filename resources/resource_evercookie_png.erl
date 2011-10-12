-module(resource_evercookie_png).

-export([]).

-include("zotonic.hrl").

-include_lib("webmachine_resource.hrl").
-include("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available()
