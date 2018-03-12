.. _hasura-project-directory-conf:


Project structure: conf/
========================

Configuration required to get a cluster ready for a project is stored as various files in this directory. When a configurations is applied on a cluster using either ``git push`` or ``conf apply`` command, these files are read to build a complete configuration object, also called ``hasura-conf``.

.. note::

   All files in this directory are rendered as a templates. Refer to :ref:`Conf files templating <conf-templating>` for more details.

You can apply configuration on a cluster either by doing ``git push`` or by using the :ref:`hasura conf apply <hasura_conf_apply>` command.

To see what the differences in configuration across clusters are, or even between the configuration applied on a cluster and the one present locally, use :ref:`hasura conf diff <hasura_conf_diff>`.

Following are the files present in this directory:

.. toctree::

   authorized-keys.yaml <authorized-keys.yaml>
   auth.yaml <auth.yaml>
   ci.yaml <ci.yaml>
   dev-mode.yaml <dev-mode.yaml>
   domains.yaml <domains.yaml>
   filestore.yaml <filestore.yaml>
   gateway.yaml <gateway.yaml>
   notify.yaml <notify.yaml>
   postgres.yaml <postgres.yaml>
   routes.yaml <routes.yaml>
   session-store.yaml <session-store.yaml>
   http-directives.conf <http-directives.conf>
