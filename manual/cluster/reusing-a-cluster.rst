==========================================
Reusing a cluster with a different project
==========================================

Sometimes, you might want to start using a cluster for an entirely different project. This section talks about the instructions to do exactly that:

  Note: You will lose all your data. Do it only if you are **absolutely sure**.

#. Add the cluster to your new project. Run the following from the ``new`` project directory.

   .. code-block:: bash

      # Adding cluster
      $ hasura cluster add clustername42 -c hasura
      # Set it as default cluster of the project
      $ hasura cluster set-default hasura

#. Reset the database of the cluster so that you can apply migrations of the new project to it.

   .. code-block:: bash

      $ hasura migration db-reset

   ::

      • This will erase all data from the database including schema. Are you sure? (yes/no)
      yes
      ✓ The database on cluster has been reset cluster=hasura [colliery88]
        # You can now remove your migrations completely by deleting the local migration files:
        $ rm migrations/*.{sql,yaml}

        # OR apply your migrations again:
        $ hasura migration apply

#. Finally, apply the configurations, migrations and microservices of the new project to cluster.

   .. code-block:: bash

      $ git add .
      $ git commit -m "Migrated the cluster to a new project"
      $ git push hasura master -f
      # Make sure to force push over existing git history
