.. _hasura-project-directory-clusters-yaml:

Project structure: clusters.yaml
================================

Info about the clusters added to this project can be found in this file. Each
cluster is defined by it's name allotted by Hasura, and an alias that the user choose.

The ``kubeContext`` mentions the name of Kubernetes context used to access the
cluster and can be used with ``kubectl`` also.

The ``config`` key denotes the location
of cluster's metadata on the cluster itself. This information is parsed and
cluster's metadata is appended while conf is rendered.

``data`` key is for holding custom variables to be used in templates.

.. code-block:: yaml

  - name: ambitious93
    alias: hasura
    kubeContext: ambitious93
    config:
      configmap: controller-conf
      namespace: hasura
    data: null

You can list all your clusters using :ref:`hasura cluster list <hasura_cluster_list>` and add any of them to the project using :ref:`hasura cluster add <hasura_cluster_add>`.

If you need to remove a cluster from a project, just remove the entry from ``clusters.yaml``.
