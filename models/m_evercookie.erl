-module(m_evercookie).

%% @doc model for pickle/depickle cookie values, mantaining list of user_cookies and accessing constants.

-behaviour(gen_model).
-export([
	m_find_value/3, m_to_list/2, m_value/2,
	is_exist/3, is_same_person/3,
	insert/3,
	observe_evercookie_postback/2,
	manage_schema/2
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


%% @doc check PK existence and push value into table.
insert(undefined, _Id, _Context) ->
    %% TODO no user_id - do nothing?
    {ok, 0};
insert(UserId, Id, Context) ->
    case is_exist(UserId, Id, Context) of
	true -> {ok, 0};
	_    -> insert_ok(UserId, Id, Context)
    end.

insert_ok(UserId, Id, Context) ->
    Props  = [{user_id, UserId}, {id, Id}],
    case z_db:insert(?T_EVERCOOKIE, Props, Context) of
	{ok,_} = Result ->
	    z_notifier:notify({evercookie_inserted, UserId, Id}, Context),
	    Result;
	E -> E
    end.


%% @doc is selected keys exist?
is_exist(UserId, Id, Context) ->
    z_db:q1("SELECT count(*) > 0 FROM " ++ ?T_EVERCOOKIE ++ " WHERE user_id = $1 AND id = $2", [UserId, Id], Context).


%% @doc get users with same ids
get_clones(UserId, Context) ->
    z_db:q(<<"SELECT DISTINCT ec1.user_id FROM ", ?T_EVERCOOKIE, " ec1 WHERE ec1.user_id /= $1 AND id IN ",
		"(SELECT ec2.id FROM ", ?T_EVERCOOKIE, "ec2 WHERE ec2.user_id = $1)">>, [UserId], Context).

%% @doc Checks if two user_ids related to the same evercookie-id. Wrapper around get_clones/2.
is_same_person(UserId1, UserId2, Context) ->
    lists:member(UserId2, get_clones(UserId1, Context)).


%% @doc install schema. 
manage_schema(install, Context) ->
    z_db:create_table(?T_EVERCOOKIE, [
	    #column_def{name=id,      type="varchar", is_nullable=false},
	    #column_def{name=user_id, type="integer", is_nullable=false}
	], Context),
    z_db:equery(<<"ALTER TABLE ", ?T_EVERCOOKIE, " ADD PRIMARY KEY (id)">>, Context),
    z_db:equery(<<"CREATE INDEX i_", ?T_EVERCOOKIE, "_user_id ON ", ?T_EVERCOOKIE, " USING btree(user_id)">>, Context),
    z_db:equery(<<"CREATE INDEX i_", ?T_EVERCOOKIE, "_id ON ",	    ?T_EVERCOOKIE, " USING btree(id)">>, Context),
    ok.
