.. .. meta::
   :description: API reference for Hasura's File  microservice. POST, GET and DELETE endpoinds for uploading, downloading and deleting files respectively.
   :keywords: hasura, docs, File, fileStore, API reference

Hasura on GKE
=============

This section describes:

1. How to setup a Hasura cluster on GKE
2. How to add that Hasura cluster to a Hasura project


Set up a GKE cluster
--------------------

Your GKE cluster needs to have a few things set up before the Hasura platform can be installed on it. The following are the necessary pieces to be set up and configured:

Navigate to your **Google Cloud Console**

1. From the **Container Engine** section, create a new GKE container cluster in whichever region you prefer. We'll select the region ``asia-east1-a`` and name the cluster as ``myco-hasura``.

2. From the **Compute Engine** section, add 3 disks:

   1. ``myco-hasura-posgtres``
   2. ``myco-hasura-redis``
   3. ``myco-hasura-filestore``

   Make sure that they are large enough to hold your data (min 10 GB each), located in the same region as your GKE cluster, the ``Source type`` option is set to ``None (blank disk)`` and preferably are SSDs.

3. From the **VPC network** section add an external static IP address (no need to attach to any particular pool). Map your domain ``myco-hasura.hasura-app.io`` to this IP.

Get the credentials for the GKE cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Set up the kube context using the ``gcloud container clusters get-credentials ...`` command from the GKE console.

Note the context set by this command since we'll be needing it later. Use ``kubectl config current-context`` (or ``get-contexts``) and note the context.

Installing the Hasura platform
------------------------------

Setup a local project
^^^^^^^^^^^^^^^^^^^^^

If you have an existing Hasura project, skip to editing the ``clusters.yaml``.

Clone the base repo from Hasura Hub into a local directory:

.. code-block:: console

   $ hasura clone hasura/base

Editing cluster.yaml
^^^^^^^^^^^^^^^^^^^^

Edit the ``clusters.yaml`` to look like this:

.. literalinclude:: clusters.yaml
   :language: yaml
   :caption: clusters.yaml
   :linenos:
   :emphasize-lines: 1,3,11,14,37,42,45

We're now ready to install the Hasura platform on the ``myco-hasura`` cluster!

Install the Hasura platform
^^^^^^^^^^^^^^^^^^^^^^^^^^^

To install the Hasura platform, run:

.. code-block:: console

   $ hasura cluster install --file clusters.yaml --provider gke --domain myco-hasura.hasura-app.io

Wait for some time for all the components of the cluster to come up. You may use ``kubectl get pods -n hasura`` to see if all the pods are up.

Set up git remotes
^^^^^^^^^^^^^^^^^^

To set up git remote and hooks, run:

.. code-block:: console

   $ hasura cluster setup -c myco-hasura

Make sure that the remotes point correctly to the cluster.

Deploy project to the cluster
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally push your project to the newly created cluster:

.. code-block:: console

   $ git push myco-hasura master
