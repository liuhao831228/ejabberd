%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_internal).

-author('alexey@process-one.net').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0, export/1, import/1,
	 import/3, plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(passwd, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                 password = <<"">> :: binary() | scram() | '_'}).

-record(reg_users_counter, {vhost = <<"">> :: binary(),
                            count = 0 :: integer() | '$1'}).

-define(SALT_LENGTH, 16).

-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    ok.

plain_password_required() ->
    false.

store_type() ->
    plain.

check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    C = c(),
    case eredis:q(C, ["GET", LUser]) of
        {ok, UserObj} ->
	    UserObj /= <<"">>;
	_ ->
	    false
    end.

check_password(User, Server, Password, Digest,
	       DigestGen) ->
    check_password(User, Server, Password).

%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, invalid_jid}
set_password(User, Server, Password) ->
    ok.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, invalid_jid} | {aborted, Reason}
try_register(User, Server, PasswordList) ->
    {error, invalid_jid}.

%% Get all registered users in Mnesia
dirty_get_registered_users() ->
    [].

get_vh_registered_users(Server) ->
    [].

get_vh_registered_users(Server,
			[{from, Start}, {to, End}])
    when is_integer(Start) and is_integer(End) ->
    get_vh_registered_users(Server,
			    [{limit, End - Start + 1}, {offset, Start}]);
get_vh_registered_users(Server,
			[{limit, Limit}, {offset, Offset}])
    when is_integer(Limit) and is_integer(Offset) ->
    [];
get_vh_registered_users(Server, [{prefix, Prefix}])
    when is_binary(Prefix) ->
    [];
get_vh_registered_users(Server,
			[{prefix, Prefix}, {from, Start}, {to, End}])
    when is_binary(Prefix) and is_integer(Start) and
	   is_integer(End) ->
    get_vh_registered_users(Server,
			    [{prefix, Prefix}, {limit, End - Start + 1},
			     {offset, Start}]);
get_vh_registered_users(Server,
			[{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
    when is_binary(Prefix) and is_integer(Limit) and
	   is_integer(Offset) ->
    [];
get_vh_registered_users(Server, _) ->
    get_vh_registered_users(Server).

get_vh_registered_users_number(Server) ->
    0.

get_vh_registered_users_number(Server,
			       [{prefix, Prefix}])
    when is_binary(Prefix) ->
    0;
get_vh_registered_users_number(Server, _) ->
    get_vh_registered_users_number(Server).

get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    C = c(),
    case eredis:q(C, ["GET", LUser]) of
        {ok, UserObj} ->
	    UserObj;
	_ ->
	    false
    end.
    

get_password_s(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    <<"">>.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    true.

%% @spec (User, Server) -> ok
%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
remove_user(User, Server) ->
    ok.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
%% @doc Remove user if the provided password is correct.
remove_user(User, Server, Password) ->
    ok.

c() ->
    Res = eredis:start_link(?REDIS_HOST, ?REDIS_PORT),
    {ok, C} = Res,
    C.    
    
