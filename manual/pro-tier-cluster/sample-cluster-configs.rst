Sample Cluster Configurations
=============================

This section has sample configurations that can be used to create or modify paid
clusters.

For details about the structure of this configuration see :doc:`reference
<./reference-clusters-yaml>`.

Hobby project - Basic
---------------------
Small single-node cluster for simple hobby or side projects (web or mobile
apps), use the following cluster configuration:

.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: sfo2
   nodes:
   - type: s-1vcpu-1gb
     labels:
       app: postgres
   volumes:
   - name: postgres
     size: 5
   - name: filestore
     size: 5
   - name: sessionstore
     size: 2


Staging environment - Basic
---------------------------
Single-node cluster for on-demand staging environments for simple apps 

.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: sfo2
   nodes:
   - type: s-2vcpu-2gb
     labels:
       app: postgres
   volumes:
   - name: postgres
     size: 10
   - name: filestore
     size: 20
   - name: sessionstore
     size: 2

Staging environment - Moderate
------------------------------
Single-node cluster for on-demand staging environments for powerful apps, with
your own volume for persistent microservices.

.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: sfo2
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


Production environment - Advanced
---------------------------------
Single-node cluster for production environments for powerful apps, with your own
volume for persistent microservices.

.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: sfo2
   nodes:
   - type: s-4vcpu-8gb
     labels:
       app: postgres
   volumes:
   - name: postgres
     size: 30
   - name: filestore
     size: 30
   - name: sessionstore
     size: 10
   # custom volume
   - name: my-volume
     size: 20
