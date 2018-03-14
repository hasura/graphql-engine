.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

Passing environment variables to a microservice
===============================================

You can provide environment variables (env vars) for a microservice to use
by editing the ``k8s.yaml`` file found at ``/microservices/<my-microservice>/k8s.yaml``
and adding a few lines. By combining this with :doc:`templating <../project/conf-files-templating>` capabilities,
you get a highly configurable environment.

Let's say you need to add three env variables as follows.
``X_API_KEY`` is a fixed value, say ``'g8yxtn754ygxn3rf'``,  ``CLUSTER_NAME`` should get the name of the cluster and ``RELEASE_MODE`` should be
``production`` for cluster with alias ``prod`` and ``dev`` for all other clusters.

The ``k8s.yaml`` for such a configuration would look like the following:

.. code-block:: yaml+jinja
   :emphasize-lines: 21-31

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
              - name: X_API_KEY
                value: 'g8yxtn754ygxn3rf'
              - name: CLUSTER_NAME
                value: '{{ cluster.name }}'
              - name: RELEASE_MODE
              {% if cluster.alias == "prod" %}
                value: production
              {% else %}
                value: dev
              {% endif %}
              imagePullPolicy: IfNotPresent
              name: app
              ports:
              - containerPort: 8080
                protocol: TCP
              resources: {}
            securityContext: {}
            terminationGracePeriodSeconds: 0
      status: {}


To deploy the changes, ``git add .`` and ``git push hasura master``.

These variables will now be available and can be consumed from the application code within the microservice as ENV variables.

