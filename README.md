# mod_offline_post

Send a push notification to a webservice when there's a message being sent to offline users.
Supports groupchat and chat.

## Install

Tested with ejabberd 17.01:
```bash
git clone <URL of this git repo>
# copy the source code folder to the module sources folder of your ejabberd
# installation (may be different on your machine - see CONTRIB_MODULES_PATH in /etc/ejaberd/ejabberdctl.cfg)
sudo mkdir -p /var/lib/ejabberd/.ejabberd-modules/sources/mod_offline_post
sudo cp -R <name of this repo>/*.spec <name of this repo>/src /var/lib/ejabberd/.ejabberd-modules/sources/mod_offline_post
# if done right ejabberdctl will list mod_offline_post as available module
ejabberdctl modules_available
# automatically compile and install mod_offline_post
ejabberdctl module_install mod_offline_post
```

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
