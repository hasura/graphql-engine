.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _hasura-notify-email:

Email
=====

This section covers the documentation about configuring Notify to send emails. Notify currently supports sending emails via `SparkPost <https://www.sparkpost.com>`_ and `SMTP <https://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol>`_.

Also, for testing out Notify, Hasura has its own test provider called ``Hasura`` which you can use to send 10 emails per day.

.. toctree::
  :maxdepth: 1

  Hasura Test Provider <hasura-test-provider>
  SparkPost <sparkpost>
  SMTP <smtp>
  Sending Emails <sending-emails>
