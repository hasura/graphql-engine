.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. meta::
   :description: Reference documentation for Hasura's ``Notify`` microservice. The service is used to send emails and sms from a Hasura project using various providers like SMTP, Spark Post, Twilio, MSG91. 
   :keywords: hasura, docs, notify, email, sms, smtp, sparkpost, twilio, msg91

======
Notify
======

The ``Notify`` service on Hasura lets you contact / notify your project users through Email and SMS.

.. note::
  
  This is an admin only micro-service. Only those users with ``admin`` role associated to them can access the API.

``Notify`` can be configured with the following providers:

* Email:

  * SMTP
  * SparkPost
* SMS:

  * Twilio
  * MSG91

Email
-----

SMTP
^^^^

If you already have an email address and want to send emails from that account itself, SMTP is the easiest one to configure and use. You can obtain SMTP settings from you email provider, like GMail and use those credentials to send emails. 

You need to configure the following options on the Hasura Project Console:

* **Hostname**: SMTP hostname for email provider (e.g. smtp.gmail.com)
* **Port (TLS)**: SMTP port number (e.g. 587)
* **Username**: SMTP username (e.g. yourusername@gmail.com) 
* **Password**: SMTP password (e.g. yourgmailpassword)

.. note ::

  If you are using GMail as your SMTP provider, you have to enable access to "Less Secure Apps" on your Google account. You can get a full explanation and how to enable it `here <https://support.google.com/accounts/answer/6010255>`_.

SparkPost
^^^^^^^^^

To start sending emails using Sparkpost, `signup <https://www.sparkpost.com/>`_ for an account and create a sending domain. You have to obtain an API key after verifying the sending domain. Sparkpost provides helpful interface to guide through the process. SparkPost's free plan should be enough to cover your intial email volume.

.. note::

  You have to own a domain for completing SparkPost setup.

Please remember that you have to setup SPF and DKIM records for your domain
to start sending emails from your domain. Otherwise, SparkPost will
reject sending emails.

Here are some Sparkpost resources for `verifying sending domain <https://support.sparkpost.com/customer/portal/articles/1933360-verify-sending-domains>`_ and setting up `SPF/DKIM: <https://www.sparkpost.com/blog/understanding-spf-and-dkim-in-sixth-grade-english/>`_

You need to configure the following option on the Hasura Project Console:

* **Sparkpost API Key**: The API key obtained from your Sparkpost account, for the sending domain. Make sure that you have checked *Transmissions: Read/Write* in API Permission section on SparkPost while creating the API key.

SMS
---

Twilio
^^^^^^

`Twilio <https://www.twilio.com>`_ is a SMS, Voice and Video provider. You can `signup <https://www.twilio.com/try-twilio>`_ for a free account and start sending SMS using Twilio. Once you have signed up and created a Twilio phone number, grab the *Account SID* and *Auth Token* from `Twilio Console <https://www.twilio.com/console/account/settings>`_.

You need to configure the following option on the Hasura Project Console:

* **Account SID**: Unique identifier for your account, obtained from the API Credentials section of Twilio Console.
* **Auth Token**: Password like secret key for the account.

MSG91
^^^^^

`MSG91 <https://msg91.com/>`_ is a SMS provider where you can `signup <https://msg91.com/signup>`_ and get an API key to use with Hasura. Take a look at `this <http://help.msg91.com/article/177-where-can-i-find-my-authentication-key>`_ guide by MSG91 to obtain your API key.

You need to configure the following option on the Hasura Project Console:

* **API Key**: Unique identifier obtained from MS91 Dashboard.

API
---

To perform the email/sms operations, the following APIs are provided:

.. toctree::
  :maxdepth: 2

  api
