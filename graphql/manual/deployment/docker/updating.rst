Updating Hasura GraphQL engine running with Docker
==================================================

This guide will help you update Hasura GraphQL engine running with Docker. This guide assumes that you already have
Hasura GraphQL engine running with Docker.

1) Check the latest release version
-----------------------------------

The current latest version is:

.. raw:: html

   <code>hasura/graphql-engine:<span class="latest-release-tag">latest</span></code>

All the versions can be found at: https://github.com/hasura/graphql-engine/releases

2) Update the Docker image
--------------------------

In the ``docker run`` command or the ``docker-compose`` command that you're running, update the image tag to this
latest version.

For example, if you had:

.. raw:: html

   <code>docker run hasura/graphql-engine:v1.0.0-alpha01 ...</code>

you should change it to:

.. raw:: html

   <code>docker run hasura/graphql-engine:<span class="latest-release-tag">latest</span> ...</code>
