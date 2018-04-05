.. .. meta::
   :description: Reference docs for clusters.yaml configuration
   :keywords: hasura, CLI, cluster,


Reference for clusters.yaml
===========================
This is the reference documentation for the structure of the ``clusters.yaml`` config file and other details.

What is clusters.yaml
---------------------

``clusters.yaml`` is a file containing the configuration of your infrastructure.
It is located at the top-level of a Hasura project directory.

The idea is to have a declarative configuration of your infrastructure so that
you can create instances of your infra **on-demand**.

As this configuration is in a file, it can be version controlled. Hence all is
required to create, replicate or move your app (Hasura project) in a new
instance is to create a cluster according to this config and push your project
to the cluster.


Structure
---------

The ``clusters.yaml`` file is a YAML file containing information about your clusters.

It contains the alias as well as the infra spec for the cluster.

Following fields make up the infra spec:

* ``version``  : A Version_ string
* ``provider`` : Provider_ name.
* ``region``   : Region_ of the cluster
* ``nodes``    : A list of Node_ of the cluster.
* ``volumes``  : A list of Volume_ of the cluster.


.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: blr1
   nodes:
   - type: s-2vcpu-4gb
     labels:
       app: postgres
   volumes:
   - name: postgres
     size: 10
   - name: filestore
     size: 30
   - name: sessionstore
     size: 5
   # custom volume
   - name: my-volume
     size: 10


.. _Version:

Version
^^^^^^^
Version of the spec. This is an internal key. It is currently ``v1``.

.. parsed-literal::
   :class: haskell-pre

   String_


.. _Provider:

Provider
^^^^^^^^
Name of the provider. Currently only ``digital-ocean`` is supported.

.. parsed-literal::
   :class: haskell-pre

   String_


.. _Region:

Region
^^^^^^
The slug (string value) of the region name of the provider.

Currently, slug of any valid Digital Ocean region. See this link for all valid
regions https://developers.digitalocean.com/documentation/v2/#list-all-regions

.. parsed-literal::
   :class: haskell-pre

   String_


.. _Node:

Node
^^^^
An object containing the type of node/machine and its labels.

.. parsed-literal::
   :class: haskell-pre

   {
       "type" : NodeType_,
       "labels": Label_

   }


.. _Volume:

Volume
^^^^^^
An object containing the name of the volume and its size in GB.

.. parsed-literal::
   :class: haskell-pre

   {
       "name" : String_,
       "size" : DiskSize_
   }

.. _NodeType:

NodeType
^^^^^^^^
The type of node (or machine) to be used. Basically, this type represents the
CPU, memory (optionally disk size) of the VM or node. The value of this field is
provider specific.

For Digital Ocean, the value of this field is any valid ``slug`` in this list:
https://developers.digitalocean.com/documentation/v2/#list-all-sizes


.. _Label:

Label
^^^^^
An object of key value pairs. You can use labels to tag your nodes.

These labels can also be used in your Kubernetes manifests as node selectors.

.. parsed-literal::
   :class: haskell-pre

   {
       String_ : String_,
       String_ : String_,
       ..
   }


.. _DiskSize:

DiskSize
^^^^^^^^
An integer value in GigaBytes (GB). This value cannot be zero.

.. _String:

String
^^^^^^
Any string value.


Examples
--------

For sample ``clusters.yaml`` configurations, see :doc:`this <./sample-cluster-configs>`.
