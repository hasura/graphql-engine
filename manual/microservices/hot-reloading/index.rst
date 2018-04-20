Hot reloading local changes to a microservice
=============================================

The biggest advantage with developing on a local machine is that you get
immediate feedback for whatever changes you make to code. Most of the languages
have some tooling around them which enables live reloading or hot reloading of
the code changes on a running instance of the program.

But, your local development environment and the production deployment
environment could be different. You might have other microservices that you need
to contact. Normal workflow is to add these variable parameters as environment
variables and set them differently on the local and production environment. But,
in certain cases, this might not be possible or could be very hard to implement.
For example, you have the Hasura API gateway resolving user cookie/token to user
id and role. Your application need only look at this id/role to enforce access
control. Doing this locally while the gateway and auth microservices are running
on the cluster could be hard. Also, any microservice can be contacted from
within the cluster using the internal endpoint without authentication.

Using Hasura, you can develop applications on the cluster just like you do on
your local system. :ref:`hasura microservice sync <hasura_microservice_sync>`
command can sync files from a local directory to any directory inside the
running container on the cluster. Using ``sync`` and hot-reloading capabilities
of the language/framework, you can make use of all the Hasura features, like
internal endpoints for contacting other microservices, id/role resolution for
cookie/tokens etc. 

This is the easiest and fastest way to develop applications on a Kubernetes
cluster.

Most of the Hasura published quickstarts on the Hub comes with documentation for
how to enable hot reloading.

Below are some examples for various languages:

.. toctree::
  :maxdepth: 1
  :titlesonly:

  nodejs
  react
  python
  golang

For more details, read the full CLI reference for :ref:`hasura microservice sync <hasura_microservice_sync>`.
