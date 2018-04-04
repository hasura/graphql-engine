.. _smtp:

Sending emails: SMTP
====================

To send emails using `SMTP <https://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol>`_, you need to configure the following options in the file ``conf/notify.yaml`` in
your Hasura project directory.

* Make SMTP to be the default provider, under ``email``

.. code-block:: yaml

  email:
    default: smtp

* Now configure your SMTP host and port, under ``email`` -> ``providers`` ->
  ``smtp``

.. code-block:: yaml

  email:
    default: smtp
    providers:
      smtp:
        hostname: "String"
        port: Int


**hostname**: SMTP hostname for email provider (e.g. smtp.gmail.com).

**port**: SMTP port number (e.g. 465) (Notify only supports SSL authentication
as of now.)

If you are following the docs along with ``conf/notify.yaml`` file open, you
might notice ``username`` and ``password`` fields. Ignore the ``username`` and
``password`` fields. We don't store the username and password directly in this
conf. Rather we store them in secrets and refer to them here.

* To configure SMTP username and password as secrets, let's update the secrets

.. code-block:: shell

  $ hasura secrets update notify.smtp.username "yourusername@example.com"
  $ hasura secrets update notify.smtp.password "youremailpassword"


.. note::
  If you are using GMail as your SMTP provider, you have to enable access to
  "Less Secure Apps" on your Google account. You can read about it `here
  <https://support.google.com/accounts/answer/6010255>`__.  GMail also enforces
  limits on emails sent (default 500/day).

.. note::
  For Hasura projects on Google Compute Engine / Google Container Engine, SMTP
  settings with standard ports like 25, 465, 587 will not work, since Google
  Compute Engine does not allow outbound connections on these ports. Hence,
  make sure that your SMTP provider have alternate ports like 2525 in case you
  want to deploy on Google Cloud. You can find more details and possible
  solutions `here
  <https://cloud.google.com/compute/docs/tutorials/sending-mail/>`__.
