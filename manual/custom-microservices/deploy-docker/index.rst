.. .. meta::
   :description: How to deploy docker images using hasura
   :keywords: hasura, manual, docker, image, custom microservice

==================================================
Deploying custom microservices using Docker images
==================================================

Not all requirements for your app may be met by the Hasura data, auth APIs.
Custom APIs and microservices will always need to be added specifically to the project.

If the microservice needed to be run has a ``docker`` image, Hasura provides an easy
way to deploy it by simply specifying the ``docker`` image.

Example: Adding a custom database browser (adminer)
---------------------------------------------------

The ``adminer`` docker image is available as
`clue/adminer <https://hub.docker.com/r/clue/adminer/>`_.

First, make sure you already have a Hasura project.  If not, quickstart the
base project from `Hasura hub <https://hasura.io/hub>`_

Create a new microservice
-------------------------
.. code-block:: shell

  $ hasura microservice generate adminer --image clue/adminer --port 80

This will create a new directory called ``adminer`` in the ``microservices``
directory with Kubernetes specs.

Create a route for the microservice
----------------------------------

Now to expose the above created microservice, we have to create a route for it.

.. code-block:: shell

  $ hasura conf generate-route adminer

::

  adminer:
   /:
     corsPolicy: allow_all
     upstreamService:
       name: adminer
       namespace: '{{ cluster.metadata.namespaces.user }}'
     upstreamServicePath: /
     upstreamServicePort: 80

     

Append the output to the ``conf/routes.yaml`` file.

This will create a route for the microservice and append it to ``conf/routes.yaml``.

Push this newly created microservice to the cluster
---------------------------------------------------

Commit and push your project to the cluster. Use ``git commit`` to commit the changes, and then ``push``:

.. code-block:: bash

   # in the root of your project directory
   $ git add microservices/adminer
   $ git commit -am "added adminer"
   $ git push hasura master

This should deploy the ``adminer`` microservice (along with other microservices) and configurations to your cluster.

If you head to ``https://adminer.<cluster-name>.hasura-app.io`` you'll see the
familiar ``adminer`` UI.


.. admonition:: Automatic SSL certificates

   The Hasura platform automatically creates Grade A SSL certificates using LetsEncrypt.

   SSL certificate creation can take a few minutes. During this time ``https://adminer.<cluster-name>.hasura-app.io``
   will not served, and you'll have to access your microservice on ``http`` instead. As soon as
   the certificate is ready, ``http://adminer.<cluster-name>.hasura-app.io`` will automatically
   start redirecting to the ``https`` version.
