Creating a paid cluster
=======================


Choose cluster configuration
----------------------------

Follow these steps to create a new cluster:

* Log in to `Hasura Dashboard <https://dashboard.hasura.io/projects>`_ and add payment details.
* Generate using hasura.io/pricing or hand-write the cluster configuration in a `clusters.yaml` file.
* Run the following CLI commands

  .. code-block:: bash

    $ hasura cluster create --type=paid
    $ hasura cluster add <cluster-name>

As an example, let us create two clusters, one for staging and one for
production and add it to a project...

