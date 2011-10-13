-module(mod_evercookie).

-author("Konstanin Nikiforov <helllamer@gmail.com>").
-mod_title("Evercookie").
-mod_description("Evercookie is extremely persistent cookie in a browser. See http://samy.pl/evercookie/").

-export([
	new/1, new/2,
	get_id/2,
	alias2name/1
    ]).

-include("zotonic.hrl").
-include("include/evercookie.hrl").


%% @doc generate new immutable cookie body
new(Context) ->
    Id = z_ids:id(16),
    new(Id, Context).

new(Id, Context) ->
    z_utils:pickle(Id, Context).


%% @doc extract id from cookie body, produces by new/1.
get_id(Cookie, Context) ->
    try
	Id = z_utils:depicke(Cookie, Context),
	{ok, Id}

    catch _:_ ->
	{error, badarg}

    end.


%% @doc convert some alias into cookie name
alias2name(png)		-> ?COOKIE_PNG;
alias2name(cache)	-> ?COOKIE_CACHE;
alias2name(etag)	-> ?COOKIE_ETAG.
