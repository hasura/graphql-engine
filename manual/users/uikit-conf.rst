.. .. meta::
   :description: Hasura Auth UI Kit Conf
   :keywords: hasura, users, auth, uikit, conf


.. _uikit-conf:

Configuration
=============

Auth UI Kit can be configured for a theme or default redirect url. In ``conf/auth.yaml`` of your hasura project,

.. code-block:: yaml

  authUIKit:
    theme: "dark"
    redirectUrl: "https://auth.{{ cluster.name }}.hasura-app.io/v1/user/info"

In case a redirectUrl is given, like the above one, it will be considered when a ``redirect_url`` query param was not given to Auth UI Kit page. You can leave it ``null`` in case you want to only redirect if a ``redirect_url`` param was given to UI Kit pages.

Options for theme are ``light`` and ``dark`` with ``dark`` being the default one.
