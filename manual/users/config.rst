Auth Microservice Configuration
===============================

.. ..todo::
   Auth configuration: this can link to hasura project/conf/auth

Workflow
--------

To change the settings of Hasura Auth running in a particular cluster:

1. Edit the file ``auth.yaml`` inside the ``conf`` directory, of the Hasura
   project associated to that cluster.

2. Apply the changes by git-committing the changes and doing git-push to that
   cluster.

Understanding Auth conf
-----------------------

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
      enabled: "true"
      defaultRoles: []
    mobile:
      enabled: "true"
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

  # Configuration related to the email provider
  email:
    # email address of the sender for verification emails
    verifyEmailFrom: getstarteduser@hasura.io
    # Name of the sender for verification emails
    verifEmailFromName: admin
    # Subject for verification emails
    verifyEmailSubject: MyAwesomeApp - Verify your account
    # Template for verification emails. HTML can be used in the template. The
    # template is a Jinja template. Leave the "{{token}}" as it is. It will be
    # used by the auth microservice to inject the actual token when sending the email.
    verifyTemplate: |
      Hi, Please click on <br/>
      https://auth.{{ cluster.name }}.hasura-app.io/v1/providers/email/verify-email?token={{ "{{token}}" }}
      to verify your email.
    # Email verification token expiry time in days
    verifyTokenExpires: "7"

    # email address of the sender for forgot password emails
    forgotPassEmailFrom: getstarteduser@hasura.io
    # Name of the sender for forgot password emails
    forgotPassEmailFromName: admin
    # Subject for forgot password emails
    forgotPassEmailSubject: MyAwesomeApp - Reset password request
    # Template for forgot password emails. HTML can be used in the template. The
    # template is a Jinja template. Leave the "{{token}}" as it is. It will be
    # used by the auth microservice to inject the actual token when sending the email.
    forgotPassTemplate: |
      Hi, <br/> Click on
      https://auth.{{ cluster.name }}.hasura-app.io/v1/providers/email/reset-password?token={{ "{{token}}" }}
      to reset your password.
    # Forgot password reset token expiry time in days
    resetTokenExpires: "7"

  # Configuration for the mobile provider
  mobile:
    # Template for the SMS that is sent. This is a Jinja template. Leave the
    # "{{otp}}" as it is. It will be used by the auth microservice to inject the
    # actual token.
    smsTemplate: |
      Verify your account with MyAwesomeApp! Your OTP is {{ "{{otp}}" }}.
    # OTP expiry time in minutes
    otpExpiryTime: "15"

  # Configuration for the mobile-password provider
  mobilePassword:
    # Template for the SMS that is sent. This is a Jinja template. Leave the
    # "{{otp}}" as it is. It will be used by the auth microservice to inject the
    # actual token.
    smsTemplate: |
      Verify your account with MyAwesomeApp! Your OTP is {{ "{{otp}}" }}.
    # OTP expiry time in minutes
    otpExpiryTime: "15"

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
  #
  # Configuration for facebook provider
  #facebook:
  #  # your facebook app client id
  #  clientId: xxxxxxxxx
  #  # your facebook app client secret
  #  clientSecret:
  #    secretKeyRef:
  #      key: auth.facebook.client.secret
  #      name: hasura-secrets
  #
  # Configuration for github provider
  #github:
  #  # your github app client id
  #  clientId: xxxxxxxxx
  #  # your github app client secret
  #  clientSecret:
  #    secretKeyRef:
  #      key: auth.github.client.secret
  #      name: hasura-secrets
  #
  # Configuration for linkedin provider
  #linkedin:
  #  # your linkedin app client id
  #  clientId: xxxxxxxxx
  #  # your linkedin app client secret
  #  clientSecret:
  #    secretKeyRef:
  #      key: auth.linkedin.client.secret
  #      name: hasura-secrets

  # Configuration for adding a custom provider
  #customProviders:
  # myCustomProvider:
  #   enabled: "true",
  #   hooks:
  #     signup: "https://mycustomprovider.test42.hasura-app.io/signup"
  #     login: "https://mycustomprovider.test42.hasura-app.io/login"
  #     merge: "https://mycustomprovider.test42.hasura-app.io/merge"
  #  defaultRoles: ["admin"]
