# ejabberd

Forked from [ejabberd_mod by Nuno Horta](https://github.com/nunohorta/ejabberd_mod)

Tested with ejabberd 15.07

## mod_offline_post

Send a push notification to a webservice when there's a message being sent to offline users.
Supports groupchat and will support chat.

### Usage

Add in ejabberd.yaml

~~~
modules:
  mod_offline_post:
    post_url: "http://..."
~~~
