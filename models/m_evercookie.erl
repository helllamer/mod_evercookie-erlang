-module(m_evercookie).

%% @doc model for pickle/depickle cookie values, mantaining list of user_cookies and accessing constants.

-behaviour(gen_model).
-export([
	m_find_value/3, m_to_list/2, m_value/2,
	install/1,
	is_exist/3, insert/3,
	observe_evercookie_postback/2
    ]).

-include("zotonic.hrl").
-include("../include/evercookie.hrl").

-define(T_EVERCOOKIE,		"evercookie").


m_find_value(new, #m{value=undefined}, Context) ->
    mod_evercookie:new(Context);
m_find_value(Atom, #m{value=undefined} = M, _Context) when is_atom(Atom) ->
    M#m{value=Atom};

m_find_value(Alias, #m{value=name}, _Context) ->
    mod_evercookie:alias2name(Alias);
m_find_value(Id, #m{value=pickle}, Context) ->
    mod_evercookie:new(Id, Context);
m_find_value(V,  #m{value=depickle}, Context) ->
    mod_evercookie:get_id(V, Context);
m_find_value(UserId, #m{value=clones}, Context) ->
    get_clones(UserId, Context);

m_find_value(_, _M, _Context) ->
    undefined.


m_to_list(_M, _Context) -> [].
m_value(_M, _Context)	-> undefined.

%% resource_evercookie_postback received some spylogs - insert it into db
observe_evercookie_postback({evercookie_postback, UserId, Id}, Context) when is_integer(UserId) ->
    insert(UserId, Id, Context);
observe_evercookie_postback(_, _Context) ->
    undefined.


%% @doc push value into table. 
insert(UserId, Id, Context) ->
    case is_exist(UserId, Id, Context) of
	true -> ok;
	_    -> Props  = [{user_id, UserId}, {id, Id}],
		{ok,_} = z_db:insert(?T_EVERCOOKIE, Props, Context)
    end.

%% @doc is selected keys exist?
is_exist(UserId, Id, Context) ->
    z_db:q1("SELECT count(*) > 0 FROM " ++ ?T_EVERCOOKIE ++ " WHERE user_id = $1 AND id = $2", [UserId, Id], Context).


%% @doc get users with same ids
get_clones(UserId, Context) ->
    z_db:q(<<"SELECT DISTINCT user_id FROM ", ?T_EVERCOOKIE, " WHERE id IN (",
		"SELECT id FROM ", ?T_EVERCOOKIE, " WHERE user_id = $1)">>, [UserId], Context).


%% install schema, ignoring errors
install(Context) ->
    z_db:ensure_table(?T_EVERCOOKIE, [
	    #column_def{name=id,      type="varchar", is_nullable=false},
	    #column_def{name=user_id, type="integer", is_nullable=false}
	], Context),
    z_db:equery("ALTER TABLE "    ++ ?T_EVERCOOKIE ++ " ADD PRIMARY KEY (id)", Context),
    z_db:equery("CREATE INDEX i_" ++ ?T_EVERCOOKIE ++ "_user_id ON " ++ ?T_EVERCOOKIE ++ " USING btree(user_id)", Context),
    z_db:equery("CREATE INDEX i_" ++ ?T_EVERCOOKIE ++ "_id ON "	     ++ ?T_EVERCOOKIE ++ " USING btree(id)", Context),
    ok.

