.. _conf-templating:

Project conf files templating
=============================

Certain files inside a Hasura project are rendered as templates, namely all the files inside :ref:`conf directory <hasura-project-directory-conf>` and :ref:`microservices/*/k8s.yaml <hasura-dir-ms-k8s.yaml>`.

Templates makes it easy for seamlessly using these files across multiple clusters. Templating also helps with the management of different kinds of clusters, for example you may have a dev, staging and production clusters where the configuration might differ slightly. The templates are written in `pongo2 <https://github.com/flosch/pongo2>`_, a Jinja2 like templating language, which follows `Django Template Syntax <https://docs.djangoproject.com/en/2.0/topics/templates/>`_.

A context variable called ``cluster`` is available to be used in the templates. This is an extension of the cluster object present in ``clusters.yaml`` file. Apart from the keys present in ``clusters.yaml``, a key called ``metadata`` with some cluster specific information will be present.

If you need to add your own variables, you can add them under the ``data`` key in ``clusters.yaml`` and this will be available as ``cluster.data``.

The entire context variable `cluster` can be viewed anytime using the command:

.. code-block:: bash

   $ hasura cluster template-context -c [cluster-alias]

A typical context is as follows:

.. code-block:: yaml

   name: ambitious93
   alias: hasura
   kubeContext: ambitious93
   config:
     configmap: controller-conf
     namespace: hasura
   metadata:
     filestore:
       volume:
         hostPath:
           path: /data/hasura.io/filestore
         name: filestore-pv
     gateway:
       externalIPs:
       - 111.222.33.44
       ports:
       - name: http
         port: 80
         protocol: TCP
         targetPort: 80
       - name: https
         port: 443
         protocol: TCP
         targetPort: 443
       - name: ssh
         port: 22
         protocol: TCP
         targetPort: 22
     namespaces:
       hasura: hasura
       user: default
     postgres:
       volume:
         hostPath:
           path: /data/hasura.io/postgres
         name: postgres-pv
     registry: null
     sessionStore:
       volume:
         hostPath:
           path: /data/hasura.io/redis
         name: redis-pv
   data: null
