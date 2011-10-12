-module(mod_evercookie).

-author("Konstanin Nikiforov <helllamer@gmail.com>").
-mod_title("Evercookie").
-mod_description("Evercookie is extremely persistent cookie in a browser. See http://samy.pl/evercookie/").

-export([
	new/1, new/2,
	get_id/2,
	atom_to_cookiename/1,
	ensure_cookies/1
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
    z_utils:depicke(Cookie, Context).


%% @doc convert atom alias to name of cookie (string)
atom_to_cookiename(png)	  -> ?COOKIE_PNG;
atom_to_cookiename(cache) -> ?COOKIE_CACHE;
atom_to_cookiename(etag)  -> ?COOKIE_ETAG.


%% @doc Ensure that all evercookies are set. Should not be used without request from js api.
ensure_cookies(Context) ->
    CookieNames	= [?COOKIE_CACHE, ?COOKIE_PNG, ?COOKIE_ETAG],
    ReqData = z_context:get_reqdata(Context),
    CookieValues = [ wrq:get_cookie_value(CookieName1, ReqData) || CookieName1 <- CookieNames ],
    CookieValue = case z_utils:coalesce(CookieValues) of
		      undefined -> binary_to_list(new(Context));
		      C		-> C
		  end,
    %% generate cookie headers:
    CookieHdrs	= [ cookie_hdr(CookieName2, CookieValue, Context) || CookieName2 <- CookieNames ],
    ReqData1	= wrq:merge_resp_headers(CookieHdrs, ReqData),
    z_context:set_reqdata(ReqData1, Context).


%% generate one Set-Cookie header string.
cookie_hdr(Name, Value, Context) ->
    CookieParams = [
	{path,		"/"},
	{domain,	z_context:cookie_domain(Context)}
    ],
    mochiweb_cookies:cookie(Name, Value, CookieParams).

