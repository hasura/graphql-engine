.. .. meta::
   :description: API reference for Hasura's File  microservice. POST, GET and DELETE endpoints for uploading, downloading and deleting files respectively.
   :keywords: hasura, docs, File, fileStore, API reference

Setting up Hasura on GKE
========================

This section describes:

1. How to setup a Hasura cluster on GKE
2. How to add that Hasura cluster to a Hasura project


Set up a GKE cluster
--------------------

Your GKE cluster needs to have a few things set up before the Hasura platform can be installed on it. The following are the necessary pieces to be set up and configured:

Navigate to your **Google Cloud Console**

1. From the **Kubernetes Engine** section, create a new Kubernetes Cluster in whichever region you prefer. We'll select the region ``asia-east1-a`` and name the cluster as ``myco-hasura``.

2. From the **Compute Engine > Disks** section, create 3 disks:

   1. ``myco-hasura-postgres``
   2. ``myco-hasura-redis``
   3. ``myco-hasura-filestore``

   Make sure that they are large enough to hold your data (min 10 GB each), located in the same region as your Kubernetes cluster, the ``Source type`` option is set to ``None (blank disk)`` and preferably are SSDs.

3. From the **VPC Network > External IP Addresses** section, reserve a static IP address in the same region as the Kubernetes cluster (no need to attach to any particular pool).

4. Map your domain (let's call it ``myco-hasura.my-domain.com``) to this IP from your DNS provider's dashboard by creating an A record.

Get the credentials for the GKE cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set up the kube context using the ``gcloud container clusters get-credentials ...`` command from the Kubernetes Engine console.

Note the context set by this command since we'll be needing it later. Use ``kubectl config current-context`` (or ``get-contexts``) and note the context.

Installing the Hasura platform
------------------------------

Create ``cluster-data.yaml``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a file named ``cluster-data.yaml`` with the content as shown below. This file defines a cluster.

.. literalinclude:: clusters.yaml
   :language: yaml
   :caption: cluster-data.yaml
   :linenos:
   :emphasize-lines: 1,2,4,10,33,38,43

We're now ready to install the Hasura platform on the ``myco-hasura`` cluster!

Install the Hasura platform
^^^^^^^^^^^^^^^^^^^^^^^^^^^

To install the Hasura platform, run:

.. code-block:: console

   $ hasura cluster install --file cluster-data.yaml --domain myco-hasura.my-domain.com

Wait for some time for all the components of the cluster to come up. You may use ``kubectl get pods -n hasura`` to see if all the pods are up.

Setup a local project
^^^^^^^^^^^^^^^^^^^^^

If you have an existing Hasura project, skip to the cluster add part.

Clone the base repo from Hasura Hub into a local directory:

.. code-block:: console

   $ hasura clone hasura/base

Add the cluster
^^^^^^^^^^^^^^^

To add this cluster to the project,

.. code-block:: console

   $ cd [project-directory]
   $ hasura cluster add --file=/path/to/cluster-data.yaml

This will add the cluster defined in ``cluster-data.yaml`` to the current project, sets up required remotes, hooks and ssh keys.

Add your SSH key
^^^^^^^^^^^^^^^^

.. code-block:: bash

   $ hasura ssh-key add -c [cluster-alias]

Deploy project to the cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally push your project to the newly created cluster:

.. code-block:: console

   $ git push myco-hasura master
