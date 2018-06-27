Hasura CLI: Troubleshooting
===========================

[1000] Directory is not a Hasura project
----------------------------------------

This error is thrown when a Hasura CLI command is executed in a directory which is not a Hasura project.

[1035] Cannot connect to cluster: network call timed out
--------------------------------------------------------

Open http://portquiz.net:3443 from your browser to check if port 3443 is blocked
on your network. If you cannot open the page in a browser, access to port 3443
is blocked on your network (typically seen in corporate/campus networks). You
can confirm this by opening the URL from a different network like mobile data.
Also check for access to port 22 by visiting  http://portquiz.net:22 CLI needs
access to these ports to function. If you cannot access these two URLs, Hasura
CLI will not work on your network and you need to contact you network
administrator to fix access to these ports..

.. _hasuractl_alternate_login:

Alternate login method
----------------------

If you are having issues with Hasura CLI's browser based login, you can try this
alternate login method. If you are getting errors like "name or address not
found" or "login failed" or "login error", you can try this out.

You can also use this method to login from a device that does not have a
browser. 

1. Visit `Hasura Dashboard <https://dashboard.hasura.io/login>`_ and login.
2. Open `https://auth.hasura.io/user/account/info
   <https://auth.hasura.io/user/account/info>`_ in the same browser/tab.

   It will show something like this:

   .. code-block:: javascript

      {
        "hasura_id": 1234,
        "mobile": null,
        "hasura_roles": [
          "user"
        ],
        "auth_token": "xxxxxxxxxxxxxxxxxxxx",
        "email": null,
        "username": "google:12345"
      }

3. Copy the value of ``auth_token``.
4. Execute the following command:

   .. code-block:: bash

      hasura login --token=<auth_token_value>

You will now be logged in.
