.. .. meta::
   :description: Collaborating on Hasura projects
   :keywords: hasura, collaboration, collaborating, collaborators, project, hasura cli, cli, hasuractl, cluster

.. _project-collaboration-manual:

.. highlight:: bash

Collaborating on a project
==========================

Many developers can work on a single Hasura project and all the clusters added to it,
since all the configuration is in files, managed by git.
A project owner can share their project git repository and add collaborators to the
clusters through `Hasura Dashboard <https://dashboard.hasura.io/clusters/>`_.

Adding collaborators to your project
------------------------------------

Assuming you already have a Hasura project and clusters,

* Push your project to an online git repository (like |github-link|) and share the URL with all collaborators to share the project source code
* Open |dashboard-link|
* Click on a cluster name that is added to the project
* Click on ``Collaborators`` tab
* Add emails of those who you want to add as collaborators to give access to the cluster
* Repeat for any other clusters

Joining as a collaborator to a project
--------------------------------------

Assuming the owner has shared the repo URL and added you as a collaborator to the clusters,

* :ref:`Login using Hasura CLI <hasura_login>` with the account having email added as collaborator
* Clone and cd into the repo
* Execute :ref:`hasura cluster list <hasura_cluster_list>`
* You should be able to see the clusters available to you and clusters added to the project
* For each cluster added to the project, make sure it is listed as available with the actual owner's email. If not, request the owner to check your email address
* Setup the project:

  .. code-block:: bash

     # this command will fetch credentials, adds your ssh key to each cluster and sets up hooks and remotes
     $ hasura setup

  .. note::

     this command is only available in this form from Hasura CLI v0.2.37 onwards

* You can now access these clusters

  .. code-block:: bash

     $ hasura cluster status -c [cluster-alias]

* Set a cluster as default

  .. code-block:: bash

     $ hasura cluster set-default [cluster-alias]

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
