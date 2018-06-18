Passing ENV variables to a microservice
========================================

There might be certain variables that your microservices need but depend on the environment the microservices are running
in. eg: API keys for dev vs prod.

You can provide environment variables (or env variables) for a microservice to use
by editing the ``k8s.yaml`` file found at ``/microservices/<my-microservice>/k8s.yaml``
and adding a few lines. By combining this with :doc:`templating <../project/conf-files-templating>` capabilities,
you get a highly configurable environment.

.. note::

   The ``k8s.yaml`` file is generally a part of the code repository and hence should not
   store any sensitive variables in it. See :doc:`Project secrets <../project/secrets/index>` to handle sensitive variables.

Let's say you need to add four ENV variables as follows:

* ``CLUSTER_NAME`` should get the name of the cluster
* ``RELEASE_MODE`` should be ``production`` for cluster with alias ``prod`` and ``dev`` for all other clusters.
* ``X_ACCNT_ID`` is a fixed value, say ``GH456C``
* ``X_API_KEY`` is a secret value, say ``x.api.key``  (See :doc:`Project secrets <../project/secrets/index>`)

The ``k8s.yaml`` for such a configuration would look like the following:

.. code-block:: yaml+jinja
   :emphasize-lines: 21-36

      apiVersion: extensions/v1beta1
      kind: Deployment
      metadata:
        creationTimestamp: null
        labels:
          app: app
          hasuraService: custom
        name: app
        namespace: '{{ cluster.metadata.namespaces.user }}'
      spec:
        replicas: 1
        strategy: {}
        template:
          metadata:
            creationTimestamp: null
            labels:
              app: app
          spec:
            containers:
            - image: hasura/hello-world:latest
              env:
              - name: CLUSTER_NAME
                value: '{{ cluster.name }}'
              - name: RELEASE_MODE
              {% if cluster.alias == "prod" %}
                value: production
              {% else %}
                value: dev
              {% endif %}
              - name: X_ACCNT_ID
                value: GH456C
              - name: X_API_KEY
                valueFrom:
                  secretKeyRef:
                    name: hasura-secrets
                    key: x.api.key
              imagePullPolicy: IfNotPresent
              name: app
              ports:
              - containerPort: 8080
                protocol: TCP
              resources: {}
            securityContext: {}
            terminationGracePeriodSeconds: 0
      status: {}


To apply the changes, commit the ``k8s.yaml`` file and ``git push hasura master`` to deploy.

These variables will now be available and can be consumed from the application code within the microservice as ENV variables.

