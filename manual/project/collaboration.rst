.. .. meta::
   :description: Collaborating on Hasura projects
   :keywords: hasura, collaboration, collaborating, collaborators, project, hasura cli, cli, hasuractl, cluster

.. _project-collaboration-manual:

.. highlight:: bash

Collaborating on a Hasura Project
=================================

Many developers can work on a single Hasura project and all the clusters added to it,
since all the configuration is in files, managed by git.
A project owner can share their project git repository and add collaborators to the
clusters through `Hasura Dashboard <https://dashboard.hasura.io/clusters/>`_.

Adding collaborators to your project
------------------------------------

Assuming you already have a Hasura project,

* Open |dashboard-link|
* Click on the cluster name that is added to the project
* Click on ``Collaborators`` tab
* Add emails of those who you want to add as collaborators
* Push your project to an online git repository like |github-link| and share the URL with collaborators

Joining as a collaborator to a project
--------------------------------------

Assuming the owner have shared the repo URL and added you as a collaborator to the clusters,

* :ref:`Login using Hasura CLI <hasura_login>` with the account having email added as collaborator
* Clone and cd into the repo
* Execute :ref:`hasura cluster list <hasura_cluster_list>`
* You should be able to see the clusters available to you and clusters added to the project
* For each cluster added to the project, make sure it is listed as available with the actual owner's email. If not, request the owner to check your email address
* Get credentials for each cluster:

  .. code-block:: bash

     $ hasura cluster get-credentials -c [cluster-alias]

* Setup the hooks and remotes:

  .. code-block:: bash

     $ hasura setup

* You can now access these clusters

  .. code-block:: bash

     $ hasura cluster status -c [cluster-alias]

* Set a cluster as default

  .. code-block:: bash

     $ hasura cluster set-default [cluster-alias]

* Add your SSH key to the cluster

  .. code-block:: bash

     $ hasura ssh-key add

* Make changes and push

  .. code-block:: bash

     $ git add [file] && git commit -m "[message]"
     $ git push [cluster-alias] master
     # you might need to force push (using --force) if two collaborators are pushing simultaneously to same cluster


.. note::

   The owner and collaborators should push changes to the upstream origin (like |github-link|) and pull changes regularly to avoid conflicts arising later.

.. |hub-link| raw:: html

   <a href="https://hasura.io/hub" target="_blank">Hasura Hub</a>

.. |dashboard-link| raw:: html

   <a href="https://dashboard.hasura.io/clusters" target="_blank">Hasura Dashboard</a>

.. |github-link| raw:: html

   <a href="https://github.com" target="_blank">GitHub</a>
