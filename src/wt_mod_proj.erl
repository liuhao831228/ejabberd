-module(wt_mod_proj).

-author('liuhao@worktile.com').

-behavior(gen_mod).

-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).

-define(REDIS_HOST, <<"127.0.0.1">>).
-define(REDIS_PORT, 6379).
-define(PRE_UID, <<"wt_user_uid:">>).
-define(PRE_RPOJ_ONLINE_USERS, <<"wt_proj_online_users:">>).
-define(PUBSUB, <<"pubsub.">>).

-define(PRE_NODE, <<"/home/">>).
-define(PROJNODE, <<"/proj/">>).
-define(NODETYPE, <<"flat">>).
%% API
-export([start_link/2, start_maker/2, get_proj_online_users/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%% Hook callbacks
-export([user_online/3, user_offline/3]).

-record(state,
	{host = <<"">>, conn}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start_maker(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_maker, JID}).

get_proj_online_users(Host, Proj) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, {get_proj_online_users, Proj}).
    

user_offline_from_proj(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {user_offline_from_proj, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    RedisHost = gen_mod:get_opt(redis_host, Opts,
                                fun(B) -> B end,
				?REDIS_HOST),
    RedisPort = gen_mod:get_opt(redis_port, Opts,
                                   fun(I) when is_integer(I), I>0 -> I end,
				   ?REDIS_PORT),
    ejabberd_hooks:add(sm_register_connection_hook, Host,
			     ?MODULE, user_online, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
			     ?MODULE, user_offline, 100),
    C = c(RedisHost, RedisPort),
    ?INFO_MSG("wt_mod_proj started, conn is :~p ~p ~n", [RedisHost, RedisPort]),
    {ok, #state{host = Host, conn = C}}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
			  ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
			  ?MODULE, user_online, 100).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({get_proj_online_users, ProjId}, _From, State) ->
    C = State#state.conn,
    Key = <<?PRE_RPOJ_ONLINE_USERS/binary, ProjId/binary>>,
    case eredis:q(C, ["SMEMBERS", Key]) of
        {ok, Members} ->
	    ?INFO_MSG("Get proj ~p memebers : ~p ~n", [ProjId, Members]),
	    {reply, Members, State};
	_Other ->
	    {reply, [], State}
    end;
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_maker, JID}, State) ->
    make_node(JID, State),
    {noreply, State};
handle_cast({user_offline_from_proj, JID}, State) ->
    %%del_node(JID, State),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
user_online(_SID, JID, _Info) ->
    start_maker(JID#jid.lserver, JID).

user_offline(_SID, JID, _Info) ->
    user_offline_from_proj(JID#jid.lserver, JID).

c(Rhost, Rport) ->
    Res = eredis:start_link(binary_to_list(Rhost), Rport),
    {ok, C} = Res,
    C.    

%%====================================================================
%% internal methods
%%====================================================================
make_node(JID, State) ->
    C = State#state.conn,    
    Uid = JID#jid.luser,
    UserKey = <<?PRE_UID/binary, Uid/binary>>,    
    case eredis:q(C, ["GET", UserKey]) of
	{ok, UserObj} ->
	    {ok, Obj, []} = rfc4627:decode(UserObj),
	    %%?INFO_MSG("make node for proj, uid : ~p, Obj: ~p ~n", [Uid, Obj]);
	    Projs = extract_projs(Obj),
	    ServerHost = State#state.host,
	    PubsubHost = <<?PUBSUB/binary, ServerHost/binary>>,
	    %%lists:foreach(fun(L) -> create_node(L, ServerHost, PubsubHost) end, Projs);
            lists:foreach(fun(L) -> append_to_online(L, Uid, C) end, Projs);
	_ ->
	    ok
    end,
    ok.

append_to_online(L, Uid, C) ->
    Key = <<?PRE_RPOJ_ONLINE_USERS/binary, L/binary>>,
    eredis:q(C, ["SADD", Key, Uid]),
    ok.

create_node(L, Sh, Ph) ->
    if not L ->
             ok;
       true ->
            NodeName = <<?PRE_NODE/binary, Sh/binary, ?PROJNODE/binary, L/binary>>,
	    mod_pubsub:create_node(Ph, Sh, NodeName, service_jid(Sh), ?NODETYPE)
    end.

extract_projs(Obj) ->
    case rfc4627:get_field(Obj, "projects") of
	{ok, Projs} ->
	    F = fun(A) -> case rfc4627:get_field(A, "pid") of
			      {ok, Pid} ->
				  Pid;
			      _ ->
				  false
			  end end,
	    [F(X) || X <- Projs ];
	_ ->
	    []
    end.

service_jid(Host) ->
    case Host of
	{U, S, _} ->
	    {jid, U, S, <<"">>, U, S, <<"">>};
	_ ->
	    {jid, <<"">>, Host,<<"">>, <<"">>, Host, <<"">>}
    end.
