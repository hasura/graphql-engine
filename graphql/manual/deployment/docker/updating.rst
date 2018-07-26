Updating GraphQL engine running with Docker
==========================================

This guide will help you update Hasura GraphQL engine running on Heroku. This guide assumes that you already have
Hasura running on Heroku.

1) Fetch the latest version
---------------------------

.. raw:: html

   The current latest version is: <code>hasura/graphql-engine:<span class="github-release-tag">latest</span></code>

All the versions can be found at: https://github.com/hasura/graphql-engine/releases

2) Update the docker image
--------------------------

In the ``docker run`` command or the ``docker-compose`` command that you're running, update the image tag to this latest version.

.. raw:: html

   For example, if you had <code>docker run hasura/graphql-engine:v1.0.0-alpha08 ...</code>, you should change it to: <code>docker run hasura/graphql-engine:<span class="github-release-tag">latest</span> ...</code>
