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

Email
-----

Any one of the following providers can be configured and enabled for sending emails.

SMTP
^^^^

If you already have an email address and want to send emails from that account itself, SMTP is the easiest one to configure and use. You can obtain SMTP settings from you email provider, like GMail and use those credentials to send emails. 

You need to configure the following options on the Hasura Project Console:

* **Hostname**: SMTP hostname for email provider (e.g. smtp.gmail.com)
* **Port (SSL)**: SMTP port number (e.g. 465) [``Notify`` only supports SSL authentication as of now. TLS is on the roadmap.]
* **Username**: SMTP username (e.g. yourusername@gmail.com) 
* **Password**: SMTP password (e.g. yourgmailpassword)

.. note ::

  If you are using GMail as your SMTP provider, you have to enable access to "Less Secure Apps" on your Google account. You can get a full explanation and how to enable it `here <https://support.google.com/accounts/answer/6010255>`_.

.. note ::
  
  For Hasura projects on Google Compute Engine / Google Container Engine, SMTP settings with standard ports like 25, 465, 587 will not work, since Google Compute Engine does not allow outbound connections on these ports. Hence, make sure that your SMTP provider have alternate ports like 2525 in case you want to deploy on Google Cloud. You can find more details and possible solutions `here <https://cloud.google.com/compute/docs/tutorials/sending-mail/>`_.

SparkPost
^^^^^^^^^

To start sending emails using SparkPost, `signup <https://www.sparkpost.com/>`_ for an account and create a sending domain. You have to obtain an API key after verifying the sending domain. SparkPost provides helpful interface to guide through the process. SparkPost's free plan should be enough to cover your initial email volume.

.. note::

  You have to own a domain for completing SparkPost setup.

Please remember that you have to setup SPF and DKIM records for your domain
to start sending emails from your domain. Otherwise, SparkPost will
reject sending emails.

Here are some SparkPost resources for `verifying sending domain <https://support.sparkpost.com/customer/portal/articles/1933360-verify-sending-domains>`_ and setting up `SPF/DKIM: <https://www.sparkpost.com/blog/understanding-spf-and-dkim-in-sixth-grade-english/>`_

You need to configure the following option on the Hasura Project Console:

* **SparkPost API Key**: The API key obtained from your SparkPost account, for the sending domain. Make sure that you have checked *Transmissions: Read/Write* in API Permission section on SparkPost while creating the API key.

SMS
---

Either Twilio or MSG91 can be configured and enabled to send SMS.

Twilio
^^^^^^

`Twilio <https://www.twilio.com>`_ is a SMS, Voice and Video provider. You can `signup <https://www.twilio.com/try-twilio>`_ for a free account and start sending SMS using Twilio. Once you have signed up and created a Twilio phone number, grab the *Account SID* and *Auth Token* from `Twilio Console <https://www.twilio.com/console/account/settings>`_.

You need to configure the following option on the Hasura Project Console:

* **Account SID**: Unique identifier for your account, obtained from the API Credentials section of Twilio Console.
* **Auth Token**: Password like secret key for the account.
* **Sender ID**: Sender number obtained from Twilio Console. Shows up as *From* at receiver's end.

MSG91
^^^^^

`MSG91 <https://msg91.com/>`_ is a SMS provider where you can `signup <https://msg91.com/signup>`_ and get an API key to use with Hasura. Take a look at `this <http://help.msg91.com/article/177-where-can-i-find-my-authentication-key>`_ guide by MSG91 to obtain your API key.

You need to configure the following option on the Hasura Project Console:

* **API Key**: Unique identifier obtained from MSG91 Dashboard.
* **Sender ID**: Sender identification (maximum 6 characters) from which the SMS will be sent.

API Reference
-------------

To perform the email/sms operations, the following APIs are provided:

.. toctree::
  :maxdepth: 2

  api
