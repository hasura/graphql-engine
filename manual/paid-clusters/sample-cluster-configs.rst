Sample Cluster Configurations
=============================

This section has sample configurations that can be used to create or modify paid clusters.


Hobby project - Basic
---------------------

Single-node cluster for simple hobby or side projects (web or mobile apps), use the following cluster congfiguration:

.. code-block:: yaml

  - name: ambitious93
    alias: hasura
    kubeContext: ambitious93
    config:
      configmap: controller-conf
      namespace: hasura
    data: null


Staging environment - Basic
---------------------------
Single-node cluster for on-demand staging environments for simple apps 

.. code-block:: yaml

  - name: ambitious93
    alias: hasura
    kubeContext: ambitious93
    config:
      configmap: controller-conf
      namespace: hasura
    data: null