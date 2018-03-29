.. _hasura-dir-conf-auth.yaml:

Project structure: conf/auth.yaml
=================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

Configuration for Hasura Auth microservice is defined in this file.

By default, this file is extensively commented to indicate usage and possible values for each key.

The following is the base structure of the file. It comes with instructions explaining the different configuration keys and how to use them.

.. code-block:: yaml

    # Configuration for Hasura Auth

    # All values in this configuration are strings, including boolean and integer
    # values.

    # Configuration for default providers
    # Each provider has the following fields:
    # `enabled` : To mark if the provider is enabled. Valid values are "true" or
    # "false".
    # `defaultRoles`: Specify the roles that get added when a user signs-up. By
    # default the user role is added (even when the list does not contain "user").
    # If you do not want any extra roles, leave it as an empty list.
    # Example:  the below two examples are same
    # defaultRoles: ["user", "admin"]
    # defaultRoles: ["admin"]
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

    # Session related configuration
    session:
      # Name of the cookie. This is usually set to your cluster's domain. No need
      # to edit this in normal circumstances.
      cookieName: {{ cluster.name }}
      # if the cookie should be sent over https only. Stick to "true".
      cookieSecure: "true"
      # The default age of a user session in seconds. Default: 181440 (3 weeks)
      sessionAge: "1814400"

    # Configuration of the UI Kit
    uiKit:
      # can be enabled or disabled; default value is "true"
      enabled: "true"
      # can configure theme to be either "light" or "dark"; default value is "dark"
      theme: "dark"
      # URL to redirect to after successfull auth; default value is null
      # use https://www.{{ cluster.name }}.hasura-app.io/ to redirect to www after login
      redirectUrl: null
    # Configuration related to the email provider
    email:
      # email address of the sender for verification emails
      verifyEmailFrom: getstarteduser@hasura.io
      # Name of the sender for verification emails
      verifEmailFromName: Admin
      # Subject for verification emails
      verifyEmailSubject: Verify your account - {{ cluster.name }}
      # Template for verification emails. HTML can be used in the template. The
      # template is a Jinja template. Leave the "{{token}}" as it is. It will be
      # used by the auth service to inject the actual token when sending the email.
      verifyTemplate: |
        Hi, Please click on <br/>
        https://auth.{{ cluster.name }}.hasura-app.io/ui/verify-email?token={{ "{{token}}" }}
        to verify your email.
      # Email verification token expiry time in days
      verifyTokenExpires: "7"

      # email address of the sender for forgot password emails
      forgotPassEmailFrom: getstarteduser@hasura.io
      # Name of the sender for forgot password emails
      forgotPassEmailFromName: Admin
      # Subject for forgot password emails
      forgotPassEmailSubject: Reset password request - {{ cluster.name }}
      # Template for forgot password emails. HTML can be used in the template. The
      # template is a Jinja template. Leave the "{{token}}" as it is. It will be
      # used by the auth service to inject the actual token when sending the email.
      forgotPassTemplate: |
        Hi, <br/> Click on
        https://auth.{{ cluster.name }}.hasura-app.io/ui/reset-password?token={{ "{{token}}" }}
        to reset your password.
      # Forgot password reset token expiry time in days
      resetTokenExpires: "7"

    # Configuration for the mobile provider
    mobile:
      # Template for the SMS that is sent. This is a Jinja template. Leave the
      # "{{otp}}" as it is. It will be used by the auth service to inject the
      # actual token.
      smsTemplate: |
        Verify your acccount with {{ cluster.name }}! Your OTP is {{ "{{otp}}" }}.
      # OTP expiry time in minutes
      otpExpiryTime: "15"
      # OTP length is optional. default value is 6
      # otpLength: "4"

    # Configuration for the mobile-password provider
    mobilePassword:
      # Template for the SMS that is sent. This is a Jinja template. Leave the
      # "{{otp}}" as it is. It will be used by the auth service to inject the
      # actual token.
      smsTemplate: |
        Verify your acccount with {{ cluster.name }}! Your OTP is {{ "{{otp}}" }}.
      # OTP expiry time in minutes
      otpExpiryTime: "15"
      # OTP length is optional. default value is 6
      # otpLength: "4"

    # Configuration for password
    password:
      # minimum length of the password allowed.
      minLength: "8"

    # Below fields are all optional
    #
    # Configuration for google provider
    #google:
    #  # list of the all the client ids generated for your Google app
    #  clientIds: ["xxxxxx", "yyyyyy"]
    #  clientSecret:
    #    secretKeyRef:
    #      key: auth.google.client_secret
    #      name: hasura-secrets
    #
    # Configuration for facebook provider
    #facebook:
    #  # your facebook app client id
    #  clientId: xxxxxxxxx
    #  # your facebook app client secret
    #  clientSecret:
    #    secretKeyRef:
    #      key: auth.facebook.client_secret
    #      name: hasura-secrets
    #
    # Configuration for github provider
    #github:
    #  # your github app client id
    #  clientId: xxxxxxxxx
    #  # your github app client secret
    #  clientSecret:
    #    secretKeyRef:
    #      key: auth.github.client_secret
    #      name: hasura-secrets
    #
    # Configuration for linkedin provider
    #linkedin:
    #  # your linkedin app client id
    #  clientId: xxxxxxxxx
    #  # your linkedin app client secret
    #  clientSecret:
    #    secretKeyRef:
    #      key: auth.linkedin.client_secret
    #      name: hasura-secrets

    # Configuration for adding a custom provider
    #customProviders:
    # myCustomProvider:
    #   enabled: "true",
    #   hooks:
    #     signup: "https://mycustomprovider.test42.hasura-app.io/signup"
    #     login: "https://mycustomprovider.test42.hasura-app.io/login"
    #     merge: "https://mycustomprovider.test42.hasura-app.io/merge"
    #     createUser: "https://mycustomprovider.test42.hasura-app.io/create-user"
    #     deleteUser: "https://mycustomprovider.test42.hasura-app.io/delete-user"
    #  defaultRoles: ["admin"]


    # The below fields are used by the platform when initializing. Please do not
    # edit these configuration
    postgres:
      database: hasuradb
      host: postgres.{{ cluster.metadata.namespaces.hasura }}
      password:
        secretKeyRef:
          key: postgres.password
          name: hasura-secrets
      port: "5432"
      user:
        secretKeyRef:
          key: postgres.user
          name: hasura-secrets
    redis:
      cred: null
      host: session-redis.{{ cluster.metadata.namespaces.hasura }}
      port: "6379"
    notifyDomain: http://notify.{{ cluster.metadata.namespaces.hasura }}
    superUser:
      password:
        secretKeyRef:
          key: auth.admin.password
          name: hasura-secrets
      username: admin
      # optional  fields
      # email: getstarteduser@hasura.io
      # mobile: 987654321

.. note::

   All values in this configuration are strings, including boolean and integer values.
