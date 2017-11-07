.. meta::
   :description: New terms and concepts in Hasura v0.15
   :keywords: hasura, v0.15, new terms and concepts


Terminologies and concepts
==========================

Following are the differences in terminologies and concepts between Hasura
platform version <= 0.14.x and >= v0.15.x.

tl;dr
-----

.. list-table::
  :header-rows: 1

  * - Description
    - Term in older version (<= v0.14.x)
    - Term in new version (>= v0.15.x)
  * - A Kubernetes cluster with Postgres, Hasura backend APIs, platform
      components, your custom code etc. hosted on a cloud provider.
    - Hasura project
    - Hasura cluster
  * - Collection of database schema, multiple-cluster configurations, source
      code etc.
    - Hasura app
    - Hasura project
  * - Custom code, docker image or Hasura backend APIs deployed and running.
    - Service
    - Microservice


Project |right-arrow| Cluster
-----------------------------------

Prior to Hasura platform v0.15 (i.e <= v0.14.x), a Kubernetes cluster with
Hasura BaaS components, PaaS components and an administrative console
installed, was called **Hasura project**. Starting from v0.15 this has been
renamed to **Hasura cluster**.

This is because a project is usually a collection of source code, database
schema and configuration etc. Not necessarily a running version of it. For
example, for the same project one might have a staging instance and a
production instance. Calling these instances separate projects does not make
sense. On the other hand, it makes sense to call them clusters. In this
example, staging and production instances would be two different clusters of
the same project.


App |right-arrow| Project
-------------------------

Previous to Hasura platform v0.15 (i.e <= v0.14.x), collection of source code,
database schema, configuration etc. was *loosely* called an **App** on Hasura.
They were not coupled together in any manner. Typically, source code was
versioned and kept separately as git repositories. The database schema and
configurations were not versioned and was only present in the running instance
of the app.

Since, Hasura platform v0.15, collection of database schema (called
migrations), and multiple clusters configuration (and optionally source code
for custom microservices) is kept together in a git repository.  This is termed
as **Hasura project**.


Service |right-arrow| Microservice
----------------------------------

Deployed custom code or a Docker image, including the installed Hasura backend
APIs were collectively called **services**, prior to Hasura v0.15.

From v0.15, they have been renamed to **microservices** to avoid confusion with
Kubernetes services.


.. |right-arrow| unicode:: U+2192
