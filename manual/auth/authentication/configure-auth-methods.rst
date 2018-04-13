Enabling/disabling authentication methods
=========================================

All the configuration related to the authentication methods a.k.a providers,
is present in :doc:`conf/auth.yaml <../../project/directory-structure/conf/auth.yaml>` file in your project drectory. The default
providers of ``auth.yaml`` look something like the snippet below:

.. snippet:: yaml
  :filename: auth.yaml

  defaultProviders:
    username:
      enabled: "true"
      defaultRoles: []
    email:
      enabled: "false"
      defaultRoles: []
    mobile:
      enabled: "false"
      defaultRoles: []
    mobile-password:
      enabled: "false"
      defaultRoles: []
    google:
      enabled: "false"
      defaultRoles: []
    facebook:
      enabled: "false"
      defaultRoles: []
    github:
      enabled: "false"
      defaultRoles: []
    linkedin:
      enabled: "false"
      defaultRoles: []

In the example above you can see that only the username provider is enabled. To
enable other providers, you simply have to change the enabled key to ``true``. For
example, if you are to enable the ``email`` provider, the above snippet will change to:

.. code-block:: yaml
  :emphasize-lines: 6

  defaultProviders:
    username:
      enabled: "true"
      defaultRoles: []
    email:
      enabled: "true"
      defaultRoles: []
    mobile:
      enabled: "false"
      defaultRoles: []
    mobile-password:
      enabled: "false"
      defaultRoles: []
    google:
      enabled: "false"
      defaultRoles: []
    facebook:
      enabled: "false"
      defaultRoles: []
    github:
      enabled: "false"
      defaultRoles: []
    linkedin:
      enabled: "false"
      defaultRoles: []

If you make any change in the ``auth.yaml``, you must run a git push to apply the configuration changes to your cluster. Just run:

.. code-block:: bash

  $ git add conf/auth.yaml
  $ git commit -m "Changed auth configuration"
  $ git push hasura master

Most auth providers might require further configuration for use cases such as

- Changing email verification template
- Changing forgot-password email template
- Changing SMS template
- Changing OTP expiry time
- Configuring client IDs and client secrets for social auth providers.

To learn about configuring auth providers, check:

- :doc:`Email <providers/email>`
- :doc:`Mobile/OTP <providers/mobile-otp>`
- :doc:`Mobile/password <providers/mobile-password>`
- :doc:`Google <providers/google>`
- :doc:`Facebook <providers/facebook>`
- :doc:`Github <providers/github>`
- :doc:`LinkedIn <providers/linkedin>`
