.. _hasura-dir-conf-http-directives.conf:

http-directives.conf
====================

.. note::

   This file is rendered as a template. Refer to :ref:`Using Templates <using-templates>` for more details.

Any extra nginx directives that can go into the 'http' section of the gateway can be mentioned here. For example, the below snippet is added by default.

.. code-block::  conf

   gzip               on;
   gzip_http_version  1.1;
   gzip_min_length    1024;
   gzip_types         text/plain text/css text/javascript application/json application/x-javascript application/javascript text/xml application/xml application/xml+rss;
   gzip_proxied       any;
   gzip_disable       msie6;
   gzip_comp_level    1;


You can find the default file at `conf/http-directives.conf <https://github.com/hasura/base/blob/master/conf/http-directives.conf>`_ in the base repo.
