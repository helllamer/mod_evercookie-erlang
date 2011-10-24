-module(action_evercookie_set_ec).

%% @doc Call cookie setter on client with some cookie value

-author("Konstanin Nikiforov <helllamer@gmail.com>").

-export([render_action/4]).

%% @doc render a call to ec.set() with some value
render_action(_TriggerId, _TargetId, [{cookie, Cookie}], Context) ->
    Script = [<<"ec.set(_ecname, '">>, Cookie, $', $), $;],
    {Script, Context}.
