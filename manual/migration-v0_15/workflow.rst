New workflow vs the old workflow
================================

This document is described in terms of new terminologies. For example, even in
the older workflow, new terms are being used. Refer :doc:`terminologies`.

.. ..todo::
  - Creating a Hasura project
  - Opening the console
  - Creating tables
  - Configuring auth/notify/filestore
  - Adding a git-push service
  - Adding a docker service
  - API gateway settings
  - SSH key / cluster access settings
  - Managing environment variables
  - Viewing logs/events


Creating a Hasura project
~~~~~~~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - No specific workflow to create a project. Usually one heads to
      `dashboard.hasura.io`_ and creates a cluster and then manually associates
      it to a project.
    - Use the ``hasura`` cli to create a project. Start with an pre-built
      project using ``hasura quickstart``, or start a fresh project: ``hasura
      clone base``. For more details refer: :doc:`../project/index`.


Creating a Hasura cluster
~~~~~~~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Go to `dashboard.hasura.io`_ and create cluster from there. Admin
      credentials to the cluster are sent over email.
    - Create a cluster by ``hasura cluster create --type=free``. Add a cluster
      to an existing project by ``hasura cluster add <cluster-name>``. No admin
      credentials are required to access the cluster. It can be accessed using
      the hasura cli. For more details refer: :doc:`../cluster/index`.


Opening the console
~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console (console.projectname42.hasura-app.io) using
      the admin credentials.
    - Open the console and connect it to any added cluster by running ``hasura
      api-console``.


Creating tables
~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console, head to Data -> Schema page. Use Create
      Table page to create a table.
    - Open the API console by using ``hasura api-console``. Head to Data ->
      Schema page.  Use Create Table page to create a table. For more details
      refer :doc:`../data/create-tables`.


Configuring auth/notify/filestore
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console, head to the specific microservice's
      configuration page. Edit values from there and save.
    - Edit the appropriate files in ``conf/{auth,notify,filestore}.yaml`` to
      change Auth, Notify and Filestore settings. To apply these changes to a
      cluster, git commit and push to that cluster. ``git commit && git push
      hasura master``.


Adding a git-push microservice
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console, head to Git Push Microservices page. Use
      Create page to create a git push microservice.
    - Use the hasura cli. Refer :doc:`../custom-microservices/index`.


Adding a docker service
~~~~~~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console, head to Git Push Microservices page. Use
      Create page to create a git push microservice.
    - Use the hasura cli. Refer :doc:`../custom-microservices/index`.


API gateway settings
~~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console, head to API Gateway page. Edit settings
      there and save.
    - Edit ``conf/{domains,http-directives,routes}.yaml`` to change any API
      Gateway settings, including routing to microservices. git commit and push
      to apply these changes to the cluster.


SSH key / cluster access settings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Managing environment variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Viewing logs/events
~~~~~~~~~~~~~~~~~~~
.. list-table::
  :header-rows: 1

  * - Old workflow (<= v0.14.x)
    - New workflow (>= v0.15.x)
  * - Login to the cluster console, head to particular microservice's page. Go
      to Logs page from there to view that micorservice's logs.
    - Using the hasura cli. ``hasura microservice logs <microservice-name>``.



.. _dashboard.hasura.io: https://dashboard.hasura.io
