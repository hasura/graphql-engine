.. _git-push-advanced:

====================
Configuring git-push
====================

By default, making any changes to your project (configuration, schema migrations, modifying microservice configuration, adding/removing microservices) can all be applied to your hasura cluster with a `git push hasura master`.

However, you can tweak this behaviour to apply each and every type of change seperately.

.. todo::

   1. How `git push hasura master` works
   2. Applying configuration changes
   3. Applying migrations
   4. Applying microservice configuration changes
   5. Pushing microservice source code changes
