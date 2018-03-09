.. _hasura-dir-conf-authorized-keys.yaml:

Project structure: conf/authorized-keys.yaml
============================================

.. note::

   This file is rendered as a template. Refer to :ref:`Using Templates <using-templates>` for more details.

Defines where SSH keys are stored on the cluster. These keys are used to give ``git push`` access to the cluster.

In the default configuration, it points to ``authorizedKeys``
of ``ssh-authorized-keys`` ConfigMap which is created
when the cluster is created.

In order to add a new SSH key, see :ref:`hasura ssh-key add <hasura_ssh-key_add>`

.. code-block:: yaml

   configMapKeyRef:
     name: ssh-authorized-keys
     key: authorizedKeys

You can find the default file at `conf/authorized-keys.yaml <https://github.com/hasura/base/blob/master/conf/authorized-keys.yaml>`_ in the base repo.
