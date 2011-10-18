-module(mod_evercookie).

-author("Konstanin Nikiforov <helllamer@gmail.com>").
-mod_title("Evercookie").
-mod_description("Evercookie is extremely persistent cookie in a browser. See http://samy.pl/evercookie/").

-export([
	init/1,
	new/1, new/2,
	get_id/2,
	alias2name/1
    ]).

-include("zotonic.hrl").
-include("include/evercookie.hrl").


init(Context) ->
    %% FIXME need test for php-gd persistence
    case os:cmd("php --version") of 
	"PHP 5"++_ -> ok;
	_	   -> ?ERROR("~p: Please install PHP 5 and php-gd lib. Php-gd is used by evercookie png generator.", [?MODULE])
    end,
    z_notifier:observe(evercookie_postback, {m_evercookie, observe_evercookie_postback}, Context),
    ok.


%% @doc generate new immutable cookie body
new(Context) ->
    Id = z_ids:id(16),
    new(Id, Context).

new(Id, Context) ->
    base64_transform(
	z_utils:pickle(Id, Context)
    ).


%% @doc extract id from cookie body, produces by new/1.
get_id(Cookie, Context) ->
    try
	Cookie1 = base64_untransform(Cookie),
	Id = z_utils:depickle(Cookie1, Context),
	{ok, Id}

    catch _:_ ->
	{error, badarg}

    end.


%% @doc convert some alias into cookie name
alias2name(png)		-> ?COOKIE_PNG;
alias2name(cache)	-> ?COOKIE_CACHE;
alias2name(etag)	-> ?COOKIE_ETAG.



%% fat crunches for cookies: they cannot contain +, =, /, etc
base64_transform(X) -> base64_transform(X, <<>>).
base64_transform(<<$+, Rest/binary>>, Acc) -> base64_transform(Rest, <<$-, Acc/binary>>);
base64_transform(<<$/, Rest/binary>>, Acc) -> base64_transform(Rest, <<$_, Acc/binary>>);
base64_transform(<<$=, Rest/binary>>, Acc) -> base64_transform(Rest, <<$., Acc/binary>>);
base64_transform(<<C,  Rest/binary>>, Acc) -> base64_transform(Rest, <<C,  Acc/binary>>);
base64_transform(<<>>, Acc) -> Acc.

base64_untransform(X) -> base64_untransform(X, <<>>).
base64_untransform([$-|T], Acc) -> base64_untransform(T, <<$+, Acc/binary>>);
base64_untransform([$_|T], Acc) -> base64_untransform(T, <<$/, Acc/binary>>);
base64_untransform([$.|T], Acc) -> base64_untransform(T, <<$=, Acc/binary>>);
base64_untransform([C |T], Acc) -> base64_untransform(T, <<C,  Acc/binary>>);
base64_untransform([], Acc) -> Acc.

