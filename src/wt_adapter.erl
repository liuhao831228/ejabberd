-module(wt_adapter).
-include("rfc4627.hrl").

-author('liuhao@worktile.com').

-behavior(gen_mod).
-behavior(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").
-include("ejabberd_http.hrl").

-define(RESP_SUCCESS, {obj, [{"code": 0}]}).

-define(SUPERVISOR, ejabberd_sup).

-export([process/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

-record(state, {host = <<"">>}).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    AdapSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, AdapSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    {ok, #state{host = Host}}.

terminate(_Reason, #state{host = Host}) ->
    ok.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%======================================================================
%% http callback
%%======================================================================

process([<<"pub">>], Request) ->
    QueryStr = Request#request.q,
    Data = Request#request.data,
    ?DEBUG("recev q:~p, d:~p ~n", [QueryStr, Data]),
    From = element(2, lists:keyfind(<<"from">>, 1, QueryStr)),
    Fjid = jlib:string_to_jid(From),
    Host = Fjid#jid.lserver,
    Type = element(2, lists:keyfind(<<"type">>, 1, QueryStr)),
    To = element(2, lists:keyfind(<<"to">>, 1, QueryStr)),
    %%Tjid = jlib:string_to_jid(To),
    case Type of
	<<"1">> -> %% project cast
	    Members = wt_mod_proj:get_proj_online_users(Host, To),
	    SendFun = fun(Uid) -> 
			      ToJid = jlib:make_jid(Uid, Host, <<"">>),
			      EventPacket = construct_event(Fjid, ToJid, QueryStr),
                              ejabberd_router:route(ToJid, Fjid, EventPacket)
		      end,
	    [SendFun(M) || M <- Members];
	<<"2">> -> %% direct to person
	    To = element(2, lists:keyfind(<<"to">>, 1, QueryStr)),
            Tjid = jlib:string_to_jid(To),
            MsgPacket = construct_msg(Fjid, Tjid, QueryStr),
	    ejabberd_router:route(Tjid, Fjid, MsgPacket)
    end,
    %%rfc4627:encode(?RESP_SUCCESS);
    "{<<\"code\">>, 0}";
process(_, _Request) ->
    "Other World".


construct_event(F, T, Q) ->
    Event = element(2, lists:keyfind(<<"event">>, 1, Q)),
    Id = randoms:get_string(),
    Attrs = [{<<"xml:lang">>, <<"en">>},
	     {<<"type">>, <<"sysevent">>},
	     {<<"to">>, jlib:jid_to_string(T)},
	     {<<"id">>, Id}],
    Body = {xmlel, <<"body">>, [], [{xmlcdata, Event}]},
    Active = {xmlel, <<"active">>, [{<<"xmlns">>, <<"http://jabber.org/protocol/chatstates">>}],[]},
    Blank = {xmlcdata, <<"\n">>},
    {xmlel, <<"message">>, Attrs, [Body, Active]}.

construct_msg(F, T, Q) ->
    Msg = element(2, lists:keyfind(<<"msg">>, 1, Q)),
    Id = randoms:get_string(),
    Attrs = [{<<"xml:lang">>, <<"en">>},
	     {<<"type">>, <<"chat">>},
	     {<<"to">>, jlib:jid_to_string(T)},
	     {<<"id">>, Id}],
    Body = {xmlel, <<"body">>, [], [{xmlcdata, Msg}]},
    Active = {xmlel, <<"active">>, [{<<"xmlns">>, <<"http://jabber.org/protocol/chatstates">>}],[]},
    Blank = {xmlcdata, <<"\n">>},
    {xmlel, <<"message">>, Attrs, [Body, Active]}.
    
    

%%======================================================================
%% API
%%======================================================================

