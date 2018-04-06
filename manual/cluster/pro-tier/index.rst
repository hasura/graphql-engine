.. .. meta::
   :description: Introduction to Hasura's pro-tier clusters/plans
   :keywords: hasura, cluster, pro-tier plans, prod, prod plans, pro tier

Pro-tier clusters
=================
..
   - What is a prod cluster?
   - Outline - creating and modifying a cluster
   - 
   - Provisioning

A pro-tier cluster is a :doc:`Hasura cluster <../index>` running on the Hasura pro-tier i.e. dedicated infrastructure managed
by Hasura. The configuration of the cluster is declarative, and hence can be
version controlled in a Hasura project.

.. note::

  * For pricing details for the pro-tier, see `Hasura pricing <https://hasura.io/pricing>`_.
  
  * Currently only Digital Ocean is supported as a cloud provider. Other cloud providers are coming soon!

See:
^^^^

.. toctree::
  :maxdepth: 1
  :titlesonly:

  create
  infra-spec
  sample-infra-specs