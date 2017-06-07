Verification & Identity Providers
=================================

Hasura Auth supports multiple identity providers. Identity provider means any
third-party which can verify the identity of the requesting user.

For example, Google can act as an identity provider by verifying the identity
of a requesting user by asking him to login with his gmail id and then giving
us some valid information about the user.

Similarly, an email address can also act as an identity provider. By sending an
unique token to the requesting user's email address and having him entered that
later, guarantees that the email is indeed owned by the user. A Mobile OTP also
works in the same way.

In the case of Hasura Auth, it can be self-provider, or it can be configured
to use any external identity providers among:

- Email
- Mobile
- Recaptcha
- Google
- Facebook
- Github
- LinkedIn


Self provider
-------------

Hasura Auth itself can also act as an identity provider.

This is the simplest use case of authentication that we are already familiar
with - username and password based authentication.

This works by in the following way: while registering, the user signs up with
an unique username and a password.  And upon login he is asked to enter both.

Along with username, email address and mobile is also supported for this basic
scheme of authentication. So instead of using username/password combination,
one can use email/password or mobile/password combinations as well.

**Note**: In this case, there is no verification of email or mobile. To know
how to configure this, see :ref:`login_using`.

Email
-----

In this case, the email address of the user acts an identity provider. It works
by sending an email to user's email address with an unique, random token.  And
then the user is asked to enter that same token to verify that he is the
legitimate owner of the email address.

.. toctree::
  :maxdepth: 2

  ./email_verification

Mobile
------

In this case, the mobile number of the user acts an identity provider. It works
by sending a one-time password (OTP) to user's given mobile number.  And
then the user is asked to enter that same OTP to verify that he is the
legitimate owner of the mobile number.

.. toctree::
  :maxdepth: 2

  ./mobile_verification

Recaptcha
---------

In this case, `Google's Recaptcha <https://developers.google.com/recaptcha/>`_
acts an identity provider, by simply asking the user to enter a recaptcha, and
thereby just verifying that it is a legitimate user and not a bot.

To know how to use it see :ref:`recaptcha` configuration.


Social Login
------------

Social login is using any of the big social network providers to verify an
identity.

Currently only Google, Facebook, Github, LinkedIn is supported.

.. toctree::
  :maxdepth: 2

  ./social_login

