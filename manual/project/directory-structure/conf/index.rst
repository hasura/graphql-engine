.. _hasura-project-directory-conf:


conf/
=====

Configuration required to get a cluster ready for a project is stored as various files in this directory. When a configurations is applied on a cluster using either ``git push`` or ``conf apply`` command, these files are read to build a complete configuration object, also called ``hasura-conf``.

.. note::

   All files in this directory are rendered as a templates. Refer to :ref:`Using Templates <using-templates>` for more details.

You can apply configuration on a cluster either by doing ``git push`` or by using the :ref:`hasura conf apply <hasura_conf_apply>` command.

To see what the differences in configuration across clusters are, or even between the configuration applied on a cluster and the one present locally, use :ref:`hasura conf diff <hasura_conf_diff>`.

Following are the files present in this directory:

.. toctree::

   authorized-keys.yaml.rst
   auth.yaml.rst
   ci.yaml.rst
   dev-mode.yaml.rst
   domains.yaml.rst
   filestore.yaml.rst
   gateway.yaml.rst
   notify.yaml.rst
   postgres.yaml.rst
   routes.yaml.rst
   session-store.yaml.rst
   http-directives.conf.rst
