-module(mod_offline_post).
-author('Diamond by BOLD').

-behaviour(gen_mod).

-export([
    start/2,
    init/2,
    stop/1,
    depends/2,
    mod_opt_type/1,
    muc_filter_message/5,
    offline_message/3
]).

-define(PROCNAME, ?MODULE).

%-include("ejabberd.hrl").
%-include("jlib.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("mod_muc_room.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_post", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, muc_filter_message, 10),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_offline_post", [] ),
    ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, muc_filter_message, 10),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message, 10),
    ok.

depends(_Host, _Opts) ->
    [].

mod_options(_Host) ->
    [].

mod_opt_type(post_url) -> fun(B) when is_binary(B) -> B end;
mod_opt_type(auth_token) -> fun(B) when is_binary(B) -> B end;
mod_opt_type(_) ->
    [post_url,auth_token].

muc_filter_message(Stanza, MUCState, RoomJID, FromJID, FromNick) ->
    PostUrl = gen_mod:get_module_opt(FromJID#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Token = gen_mod:get_module_opt(FromJID#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Type = xmpp:get_type(Stanza),
    BodyTxt = xmpp:get_text(Stanza#message.body),

    ?DEBUG("Receiving offline message type ~s from ~s to ~s with body \"~s\"", [Type, FromJID#jid.luser, RoomJID#jid.luser, BodyTxt]),

    _LISTUSERS = lists:map(
        fun({_LJID, Info}) ->
            binary_to_list(Info#user.jid#jid.luser) ++ ".."
        end,
        dict:to_list(MUCState#state.users)
    ),
    ?DEBUG(" #########    GROUPCHAT _LISTUSERS = ~p~n  #######   ", [_LISTUSERS]),

    _AFILLIATIONS = lists:map(
        fun({{Uname, _Domain, _Res}, _Stuff}) ->
            binary_to_list(Uname) ++ ".."
        end,
        dict:to_list(MUCState#state.affiliations)
    ),
    ?DEBUG(" #########    GROUPCHAT _AFILLIATIONS = ~p~n  #######   ", [_AFILLIATIONS]),

    _OFFLINE = lists:subtract(_AFILLIATIONS, _LISTUSERS),
    ?DEBUG(" #########    GROUPCHAT _OFFLINE = ~p~n  #######   ", [_OFFLINE]),

    if
        BodyTxt /= "", length(_OFFLINE) > 0 ->
            Sep = "&",
            Post = [
                "type=groupchat", Sep,
                "to=", RoomJID#jid.luser, Sep,
                "from=", FromJID#jid.luser, Sep,
                "offline=", _OFFLINE, Sep,
                "nick=", FromNick, Sep,
                "body=", BodyTxt, Sep,
                "access_token=", Token
            ],
            ?DEBUG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
            httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
            Stanza;
        true ->
            Stanza
    end.

%%
%% Forked from https://github.com/adamvduke/mod_interact/
%%
offline_message(From, To, #message{type = Type, body = Body}) ->
    Token = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    PostUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    BodyTxt = xmpp:get_text(Body),
    
    ?DEBUG("Receiving offline message type ~s from ~s to ~s with body \"~s\"", [Type, From#jid.luser, To#jid.luser, BodyTxt]),
    if
        (Type == chat) and (BodyTxt /= "") ->
            Sep = "&",
            Post = [
                "type=chat", Sep,
                "to=", To#jid.luser, Sep,
                "from=", From#jid.luser, Sep,
                %"body=", binary_to_list(Body), Sep,
                "body=", BodyTxt, Sep,
                "access_token=", Token
            ],
            ?DEBUG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
            httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
            ok;
        true ->
            ok
    end.
