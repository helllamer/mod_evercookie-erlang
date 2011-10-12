-module(mod_evercookie).

-author("Konstanin Nikiforov <helllamer@gmail.com>").
-mod_title("Evercookie").
-mod_description("Evercookie is extremely persistent cookie in a browser. See http://samy.pl/evercookie/").

-export([
	new/1, new/2,
	get_id/2,
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
    try
	Id = z_utils:depicke(Cookie, Context),
	{ok, Id}

    catch _:_ ->
	{error, badarg}

    end.


%% @doc Ensure that all evercookies are set. Should not be used without request from js api.
ensure_cookies(Context) ->
    ReqData = z_context:get_reqdata(Context),
    CookieValue = case wrq:get_cookie_value(?COOKIE, ReqData) of
		      undefined -> binary_to_list(new(Context));
		      C		-> C
		  end,
    %% generate cookie headers:
    CookieParams = [ 
	        {path,          "/"},
		{domain,	z_context:cookie_domain(Context)}
    ],
    CookieHdr	= mochiweb_cookies:cookie(?COOKIE, CookieValue, CookieParams),
    ReqData1	= wrq:merge_resp_headers([CookieHdr], ReqData),
    z_context:set_reqdata(ReqData1, Context).

