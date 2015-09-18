# mod_offline_post

Send a push notification to a webservice when there's a message being sent to offline users.
Supports groupchat and chat.

## ejabberd

Tested with ejabberd 15.07

## Usage

Add in ejabberd.yaml

~~~
modules:
  mod_offline_post:
    post_url: "http://..."
    auth_token: "AUTHENTICATION_TOKEN"
~~~

## Credits

Forked from [ejabberd_mod by Nuno Horta](https://github.com/nunohorta/ejabberd_mod) and [mod_interact by Adam Duke](https://github.com/adamvduke/mod_interact/)
