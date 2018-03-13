.. _hasura-dir-ms-k8s.yaml:

Project structure: microservices/*/k8s.yaml
===========================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

A microservice on Hasura is defined as a `Kubernetes Deployment <https://kubernetes.io/docs/concepts/workloads/controllers/deployment/>`_ and a corresponding `Kubernetes Service <https://kubernetes.io/docs/concepts/services-networking/service/>`_. Both of these objects are saved in a single file, ``k8s.yaml`` as a Kubernetes List object.

Whenever you ``git push`` or use :ref:`hasura ms apply <hasura_microservice_apply>`, this file is read and the objects are created on the cluster. You can edit this file to add secrets and other environment variables for the microservice.

Docker Image
------------

Docker image to be used by the microservice is given as ``image:`` key in ``k8s.yaml``. You can change it, commit and push to get the new image deployed.

For microservices with continuous integration enabled, i.e. their name appears in :ref:`ci.yaml <hasura-dir-conf-ci.yaml>`, the ``image`` key is ignored. The image is built on the cluster using :ref:`Dockerfile <hasura-dir-ms-dockerfile>` and the image is updated automatically, post git push.


Secrets
-------

Checkout :ref:`this page <hasura-secrets-manual>` for more info on using secrets.

Environment Variables
---------------------

You can provide environment variables (env vars) for microservices to use by editing this file and adding a few lines. By combining this with templating capabilities, you get a highly configurable environment.

Let's say you need to add two variables called ``CLUSTER_NAME`` and ``RELEASE_MODE``. Cluster name should get the name of the cluster and release mode should be ``production`` for cluster with alias ``prod`` and ``dev`` for all other clusters. Deployment section in ``k8s.yaml`` for such a configuration would look like the following:

.. code-block:: yaml+jinja
   :emphasize-lines: 21-29

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
           imagePullPolicy: IfNotPresent
           name: app
           ports:
           - containerPort: 8080
             protocol: TCP
           resources: {}
         securityContext: {}
         terminationGracePeriodSeconds: 0
   status: {}

These variables can be consumed from the application code within the microservice.

For instructions on how to access Postgres database from within a microservice, refer to :ref:`Connecting to Hasura PostgreSQL database from microservice <connecting-to-postgres>`.

Ports
-----

The port on which the microservice is listening at should be denoted in ``k8s.yaml``. A port and a target port is mentioned in the ``ports`` section along with protocol used and a name. Multiple ports can also be defined in this manner. Service section in ``k8s.yaml`` for a microservice with port 8080 on the microservice mapped to port 80 would look like the following:

.. code-block:: yaml
   :emphasize-lines: 11-15

   apiVersion: v1
   kind: Service
   metadata:
     creationTimestamp: null
     labels:
       app: app
       hasuraService: custom
     name: app
     namespace: '{{ cluster.metadata.namespaces.user }}'
   spec:
     ports:
     - name: http-port
       port: 80
       protocol: TCP
       targetPort: 8080
     selector:
       app: app
     type: ClusterIP
   status:
     loadBalancer: {}


.. note::
   
   If another microservice wants to contact this one, it would do so by making an API call to ``http://app.default:80``

You can find sample file for a python-flask microservice at `hasura/hello-python-flask/microservices/app/k8s.yaml <https://github.com/hasura/hello-python-flask/blob/master/microservices/app/k8s.yaml>`_.
