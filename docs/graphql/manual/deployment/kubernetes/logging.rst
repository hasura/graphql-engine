Hasura GraphQL engine server logs (Kubernetes)
==============================================

You can check logs of Hasura GraphQL engine deployed on Kubernetes by checking the logs of the
GraphQL engine deployment:

.. code-block:: bash

  $ kubectl logs -f deploy/graphql-engine


See https://kubernetes.io/docs/concepts/cluster-administration/logging for more details on logging in Kubernetes.