.. _hasura-dir-ms-k8s.yaml:

Project structure: microservices/\*/k8s.yaml
============================================

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

Check out :doc:`this page <../../secrets/index>` for more info on using secrets.

Environment Variables
---------------------

Check out :doc:`this page <../../../microservices/env-variables>` for more info on passing ENV variables to a microservice.

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
