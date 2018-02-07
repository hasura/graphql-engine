.. _git-push-advanced:

====================
Configuring git-push
====================

By default, making any changes to your project (configuration, schema
migrations, modifying microservice configuration, adding/removing
microservices) can all be applied to your Hasura cluster with a ``git push
hasura master`` command.

However, you can tweak this behaviour to apply each and every type of change
separately.


How ``git push hasura master`` works
------------------------------------
``git push hasura master`` is a wrapper command for applying multiple changes.

It applies the following changes:

1. Applies cluster configuration changes.
2. Applies database schema migrations.
3. Applies microservices Kubernetes spec changes.
4. Pushes custom microservices source code (if it exists).

So, whenever a change is made to the project in database schema, cluster
configuration, or microservice config changes; commit the changes and execute
``git push hasura master`` to apply all of the changes.

Pushing to a cluster not named ``hasura``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The above also assumes that you have a cluster which is aliased to ``hasura``.
This might not be the case in a complex project. You can see all your clusters
by running ``hasura cluster list``.

If you have a different cluster name, use the cluster name or alias as the
remote in the git push command. For example, if your cluster is aliased as
``staging`` then use ``git push staging master`` to apply all changes.

Under the hood, there is a git pre-push hook that runs that does all of the
above.

Applying configuration changes
------------------------------
To only apply cluster configuration changes, i.e any change inside ``conf``
directory, run the following command:

.. code-block:: bash

  $ hasura conf apply # optionally -c <cluster-alias> 

Applying migrations
-------------------
To only apply database schema changes, i.e any change inside ``migrations``
directory, run the following command:

.. code-block:: bash

  $ hasura migration apply # optionally -c <cluster-alias> 


Applying microservice configuration changes
-------------------------------------------
To only apply microservice configuration changes, i.e any change inside to the
``k8s.yaml`` file inside a microservice directory, run the following command:

.. code-block:: bash

  $ hasura microservice apply # optionally -c <cluster-alias> 

This can be port changes, Docker image changes, change environment variables
etc.

Pushing microservice source code changes
----------------------------------------
If you want to push your custom microservice code to deploy the latest
changes, but do not want to apply all changes of the project (like
configuration, migrations etc.), then you can use:

.. code-block:: bash

  $ git push <cluster-name/alias> master --no-verify
