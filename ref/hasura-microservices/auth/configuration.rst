.. meta::
   :description: Reference docs for Hasura Auth service's config params in the console- settings for user account, Email/Mobile, Facebook, Google,  LinkedIn, Recaptca. 
   :keywords: hasura, docs, auth, configuration, console, zccount settings, email configuration, mobile configuration, SparkPost configuration, MSG91 configuration, facebook configuration, google configuration, linkedin configuration, Recaptcha configuration

Configuration
=============

There are various configuration parameters through which various aspects of
Hasura Auth can be configured.

The configuration console, can be found in the Hasura project console.

Account
-------

In this section, you can configure various aspects of a user account.


.. _login_using:

Login Using
+++++++++++

This is where you can configure what your users use for logging in.

Valid values include: username, email, mobile, and the social login providers.

Check all the fields that you want to enable for login.

You can enable email or mobile based login, and by default their verification
will be disabled. You enable it in :ref:`verif_req_for`.

Note: Google, Facebook, LinkedIn logins some extra configuration. So you can't
enable them unless you have filled those specific details. Once, the specific
details are filled (like Google login details for example), that login will be
checked automatically.


.. _verif_req_for:

Verification required for
+++++++++++++++++++++++++

You can configure verification of identities of your users. For example,
you can enable email or mobile verification.

If you enable recaptcha, basically you can use recaptcha in your sign up forms.

Check all the fields that you want to be verified.

**NOTE**: Most verifications will require additional setup. For example, to
enable email verification you need to provide a valid SparkPost API Key. See
:ref:`email`. Same goes for mobile and social logins.

Other Settings
++++++++++++++

* **Allow user to delete own account**:  If this is enabled, users are allowed
  delete their own accounts. Default Value: Not enabled.

* **Email verification expires**: The time in days, after which the verification
  email will expire. Default Value: 5.

* **Password reset email expires**: The time in days, after which the password
  reset email will expire. Default Value: 2.

* **Mobile OTP expires**: The time in minutes, after which the verification SMS
  will expire. Default Value: 15.

* **Minimum password length**: The minimum password length that you want for your
  user accounts. Default Value: 8.

Hasura Auth will also monitor failed attempts for login, and can block those
accounts for specific time.

* **Maximum login attempts**: The no. of failed login attempts after which the
  account will be blocked. Default Value: 5.

* **Cooloff Time**: The time, in seconds, for which the account will be blocked.
  After the specified time, the account will be unlocked again.
  Default Value: 600 (that is 10 minutes).


.. _email:

Email
-----

Right now Hasura Auth supports only `SparkPost <https://www.sparkpost.com/>`_.
But very soon, it will support `Mandrill <https://mandrill.com>`_ and `AWS
<https://aws.amazon.com/ses/>`_.

To start sending emails, signup for `SparkPost <https://www.sparkpost.com/>`_.
Then create a sending domain, verify the domain and then obtain an API key.
SparkPost provides helful interface to guide through the process.

Please remember that you have to setup SPF and DKIM records for your domain
to start sending emails from your domain. Otherwise, any email provider will
reject sending emails.

SparkPost resources for setting up SPF/DKIM:
`<https://support.sparkpost.com/customer/portal/articles/1933360-verify-sending-domains>`_
`<https://www.sparkpost.com/blog/understanding-spf-and-dkim-in-sixth-grade-english/>`_


* **SparkPost API Key**: The API key obtained from your SparkPost account.

* **Sender email address**: Email address of the sender.

* **Sender name**: Name of the sender.

* **Verify email template**: The email template which will be sent for the
  verification email. This template can be plain text or HTML. In this template
  you can use the special variable ``%(token)s``, and that will get substituted
  by the actual token when sending the email.

  An example of the template::

    <p>
      <h3>Hi!</h3>
      Please click on http://myawesomeapp.com/verify-email?token=%(token)s to verify your email.
    </p>

  Where the ``/verify-email`` endpoint is in your application, which can
  retrieve the token, and do further processing.


.. _mobile:

Mobile
------

For text messages, Hasura right now supports `MSG91 <https://msg91.com/>`_. But
there are plans in the future to support more providers.

To start using mobile verification, or any text message related auth flow,
please create an account in `MSG91 <https://msg91.com/>`_. Then obtain an API
key.

* **MSG91 Key**: The API key obtained.

* **MSG91 Sender**: Name of the sender.

* **Verify SMS template**: The SMS template which will be sent for the
  verification message. In this template
  you can use the special variable ``%(otp)s``, and that will get substituted
  by the actual OTP when sending the text message.

  An example of the template::

    Hi! Verify your phone for your MyAwesomeApp account. Your OTP is %(otp)s.


Google
------

If you are using Google login, then after creating a Google application obtain
the Client ID and Secret.

* **CLIENT_ID**: The Client ID obtained after creating the application.

* **CLIENT_SECRET**: The Client secret.


Facebook
--------

If you are using Facebook login, then after creating a Facebook application obtain
the App ID and Secret.

* **APP ID**: The App ID obtained after creating the application.

* **APP SECRET**: The App secret.


LinkedIn
--------

If you are using LinkedIn login, then after creating a LinkedIn application obtain
the Client ID and Secret.

* **CLIENT_ID**: The Client ID obtained after creating the application.

* **CLIENT_SECRET**: The Client secret.


.. _recaptcha:

Recaptcha
---------

If you want to use recaptcha in your sign up forms, please follow this link to
setup recaptcha for your domain/site.

Setting up recaptcha: `<https://developers.google.com/recaptcha/intro>`_

* **RECAPTCHA SECRET KEY**: Obtain the value of your recaptcha secret, and paste it
  here.
