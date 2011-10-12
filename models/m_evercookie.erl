-module(m_evercookie).

%% @doc model for pickle/depickle cookie values and accessing constants.
-behaviour(gen_model).
-export([
	m_find_value/3, m_to_list/2, m_value/2
    ]).

-include("zotonic.hrl").
-include("../include/evercookie.hrl").


m_find_value(new, #m{value=undefined}, Context) ->
    mod_evercookie:new(Context);
m_find_value(Atom, #m{value=undefined} = M, _Context) when is_atom(M) ->
    M#m{value=Atom};

m_find_value(Id, #m{value=pickle}, Context) ->
    mod_evercookie:new(Id, Context);
m_find_value(V,  #m{value=depickle}, Context) ->
    mod_evercookie:get_id(V, Context);
m_find_value(N,    #m{value=name}, _Context) -> 
    mod_evercookie:atom_to_cookiename(N);

m_find_value(_, _M, _Context) ->
    undefined.


m_to_list(_M, _Context) -> [].
m_value(_M, _Context)	-> undefined.
