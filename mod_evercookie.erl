-module(mod_evercookie).

-author("Konstanin Nikiforov <helllamer@gmail.com>").
-mod_title("Evercookie").
-mod_description("Evercookie is extremely persistent cookie in a browser. See http://samy.pl/evercookie/").

-export([new/1, new/2, get_id/2, atom_to_cookiename/1]).

-include("include/evercookie.hrl").

%% @doc generate new immutable cookie body
new(Context) ->
    Id = z_ids:id(16),
    new(Id, Context).

new(Id, Context) ->
    z_utils:pickle(Id, Context).


%% @doc extract id from cookie body, produces by new/1.
get_id(Cookie, Context) ->
    z_utils:depicke(Cookie, Context).


atom_to_cookiename(png)	  -> ?COOKIE_PNG;
atom_to_cookiename(cache) -> ?COOKIE_CACHE;
atom_to_cookiename(etag)  -> ?COOKIE_ETAG.


%% @doc ensure that all evercookies are already set.
%observe_auth_logon(auth_logon, Context, _Context) ->
    %ReqData = z_context:get_reqdata(Context),
    %CookieValue = case wrq:get_cookie_value(?COOKIE_CACHE, ReqData) of
	%undefined -> case wrq:get_cookie_value(?COOKIE_PNG, ReqData) of
			%%undefined -> case wrq:get_cookie_value(?COOKIE_ETAG, ReqData) of
					%undefined -> new(Context);
					%C3 -> C3
				     %end
			%C2 -> C2
		     %end;
	%C1 -> C1
    %end,
    %% 
