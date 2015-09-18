-module(mod_offline_post).
-author('Diamond by BOLD').

-behaviour(gen_mod).

-export([
    start/2,
    init/2,
    stop/1,
    post_muc/5
]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("mod_muc_room.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_post_muc", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, post_muc, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_post_muc", [] ),
    ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, post_muc, 10),
    ok.

post_muc(Stanza, MUCState, RoomJID, FromJID, FromNick) ->
    PostUrl = gen_mod:get_module_opt(FromJID#jid.lserver, ?MODULE, post_url, fun(S) -> iolist_to_binary(S) end, list_to_binary("")),
    Body = xml:get_path_s(Stanza, [{elem, list_to_binary("body")}, cdata]),

    _LISTUSERS = lists:map(
        fun({_LJID, Info}) ->
            binary_to_list(Info#user.jid#jid.luser) ++ ".."
        end,
        dict:to_list(MUCState#state.users)
    ),
    ?INFO_MSG(" #########    GROUPCHAT _LISTUSERS = ~p~n  #######   ", [_LISTUSERS]),

    _AFILLIATIONS = lists:map(
        fun({{Uname, _Domain, _Res}, _Stuff}) ->
            binary_to_list(Uname) ++ ".."
        end,
        dict:to_list(MUCState#state.affiliations)
    ),
    ?INFO_MSG(" #########    GROUPCHAT _AFILLIATIONS = ~p~n  #######   ", [_AFILLIATIONS]),

    _OFFLINE = lists:subtract(_AFILLIATIONS, _LISTUSERS),
    ?INFO_MSG(" #########    GROUPCHAT _OFFLINE = ~p~n  #######   ", [_OFFLINE]),

    if
        Stanza /= "", length(_OFFLINE) > 0 ->
            Sep = "&",
            Post = [
                "to=", RoomJID#jid.luser, Sep,
                "from=", FromJID#jid.luser, Sep,
                "offline=", _OFFLINE, Sep,
                "nick=", FromNick, Sep,
                "body=", binary_to_list(Body)
            ],
            ?INFO_MSG("Sending post request to ~s with body \"~s\"", [PostUrl, Post]),
            httpc:request(post, {binary_to_list(PostUrl), [], "application/x-www-form-urlencoded", list_to_binary(Post)},[],[]),
            Stanza;
        true ->
            Stanza
    end.
