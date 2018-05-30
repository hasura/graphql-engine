Sample cluster infra specs
==========================

This section has sample configurations that can be used to create or modify :doc:`pro-tier
clusters <index>`.

For details about the structure of this configuration see :doc:`infra-spec`.

Hobby project - Basic
---------------------
Small single-node cluster for simple hobby or side projects (web or mobile
apps), use the following cluster configuration:

.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: sfo2
   nodes:
   - type: s-1vcpu-2gb
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
   - type: s-2vcpu-4gb
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
Multi-node cluster for on-demand staging environments for powerful apps, with
your own volume for persistent microservices.

.. code-block:: yaml

   version: v1
   provider: gke
   zone: us-west1-b
   nodes:
   - type: n1-standard-2
     labels:
       app: postgres
   - type: n1-standard-2
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
Multi-node cluster for production environments for powerful apps, with your own
volume for persistent microservices.

.. code-block:: yaml

   version: v1
   provider: gke
   zone: us-west1-b
   nodes:
   - type: n1-standard-4
     labels:
       app: postgres
   - type: n1-standard-4
   volumes:
   - name: postgres
     size: 50
   - name: filestore
     size: 50
   - name: sessionstore
     size: 10
   # custom volume
   - name: my-volume
     size: 20
