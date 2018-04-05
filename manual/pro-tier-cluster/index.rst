.. .. meta::
   :description: Introduction to Hasura's pro-tier clusters/plans
   :keywords: hasura, cluster, pro-tier plans, prod, prod plans, pro tier

Pro-Tier Cluster
================
..
   - What is a prod cluster?
   - Outline - creating and modifying a cluster
   - 
   - Provisioning

A pro-tier cluster is a Hasura cluster running on the pro-tier i.e. dedicated infrastructure managed
by Hasura. The configuration of the cluster is declarative, and hence **can be
version controlled in a Hasura project**.



.. note::

  * Details of the Pro-Tier can be found here - `Hasura pricing <https://hasura.io/pricing>`_.
  
  * Currently only Digital Ocean is supported as a cloud provider. Other cloud providers are coming soon!


.. toctree::
  :maxdepth: 1
  :titlesonly:

  create-pro-tier-cluster
  sample-cluster-configs
  manage-pro-tier-cluster
  .. cluster-maintenance
  manage-billing
  free-pro-migration
  reference-clusters-yaml
