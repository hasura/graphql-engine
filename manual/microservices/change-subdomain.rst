Changing sub-domain of a microservice
=====================================

A microservice can be contacted externally (ie: from outside a cluster) on a subdomain which is configurable.
The subdomain of a microservice is generally the same as the name of the microservice but it can be changed.

To change the subdomain on which a microservice is exposed you need to modify the :doc:`conf/routes.yaml <../project/directory-structure/conf/routes.yaml>` file
in the project directory.

A typical routing config block looks as follows:

.. code-block:: yaml
   :emphasize-lines: 1,5

   myapp:  # the sub-domain
     /:
       corsPolicy: allow_all
       upstreamService:
         name: myapp  #  the microservice name
         namespace: '{{ cluster.metadata.namespaces.user }}'
       upstreamServicePath: /
       upstreamServicePort: 80

To serve the microservice <myapp> at the sub-domain <newsubdom>, update the config block as follows:

.. code-block:: yaml
   :emphasize-lines: 1,5

      newsubdom:  # the sub-domain
        /:
          corsPolicy: allow_all
          upstreamService:
            name: myapp  #  the microservice name
            namespace: '{{ cluster.metadata.namespaces.user }}'
          upstreamServicePath: /
          upstreamServicePort: 80

To apply the changes, commit ``conf/routes.yaml`` and ``git push hasura master`` to deploy the changes