Installing Hasura on Docker for Desktop
=======================================

If you have a Mac or a Windows machine, you can install Docker for Mac or Docker
for Windows (from Edge channel) to get a Kubernetes cluster. This is a
zero-configuration single node Kubernetes cluster. 

.. note::

   Kubernetes is only available in ``Edge`` releases.

Step 1: Install Docker for Mac/Windows
--------------------------------------

MacOS
^^^^^

Install Docker CE for Mac (Edge) from `Docker Store <https://store.docker.com/editions/community/docker-ce-desktop-mac>`_.

Windows
^^^^^^^

Install Docker CE for Windows (Edge) from `Docker Store <https://store.docker.com/editions/community/docker-ce-desktop-windows>`_.

.. note::

   Requires Microsoft Windows 10 Professional or Enterprise 64-bit

Step 2: Enable Kubernetes
-------------------------

Once Docker for Desktop (Edge) is installed, click the Docker icon on the system
tray/panel, goto ``Preferences`` and then to the ``Kubernetes`` tab. Check the
box that says ``Enable Kubernetes``.

Click ``Apply``.

Step 3: Create data directory
-------------------------------

Docker for Desktop mounts certain directories from the host on to the Kubernetes
node. You can create Hasura data directory in any of these paths. These paths
are listed under ``File Sharing`` tab in Docker ``Preferences``.

On a Mac, the typical paths are ``/Users``, ``/private``, ``/Volumes`` and
``/tmp``. You can add more paths here. These directories directly map to the
same paths inside the VM.

Create a directory in any of these paths:

.. code-block:: bash

   mkdir /Users/hasura-data


Step 4: Install Hasura platform
-------------------------------

Now, you can use Hasura CLI to install Hasura platform on this cluster.

.. code-block:: bash

   hasura cluster install \
     --name=docker-desktop-hasura-cluster \
     --provider=docker-for-desktop \
     --kube-context=docker-for-desktop \
     --data-path=/Users/hasura-data \
     --domain=127.0.0.1.xip.io

This could take around 5 minutes depending on your internet connection
bandwidth.

.. note::

   Hasura platform requires a domain associated with a cluster for the subdomain
   based routing to work. For Docker for Desktop, we are making use of a service called
   `xip.io <http://xip.io>`_ which map domains with IP addresses in them to the given
   IP. We'll be creating a Kubernetes ServiceType LoadBalancer and by default it
   binds on ``localhost, 127.0.0.1``. If you have any other services listening
   on port 80, 443 and 2022 installation might fail.

Step 4: Add this cluster to a project
-------------------------------------

Clone a new Hasura project using :ref:`hasura clone <hasura_clone>` or ``cd``
into an existing project. You can then use :ref:`hasura cluster add
<hasura_cluster_add>`  command to add this cluster to the project.

.. code-block:: bash

   hasura cluster add docker-desktop-hasura-cluster \
     -c dfd \
     --kube-context=docker-for-desktop

This command will add the cluster called ``docker-desktop-hasura-cluster`` (name
we used with ``--name`` flag earlier in the install command), that can be
contacted using the kube context ``docker-for-desktop``, to the current project
with an alias ``dfd``. 

Step 5: Configure domains in the project
----------------------------------------

Your current Hasura project is most likely to have the domain configured as ``"{{
cluster.name }}.hasura-app.io"`` in :ref:`conf/domains.yaml
<hasura-dir-conf-domains.yaml>`. This domain will only work for clusters
provisioned through Hasura, not for user provisioned ones. Hence, you need to
edit this file and change the domain configuration.

Edit :ref:`conf/domains.yaml <hasura-dir-conf-domains.yaml>` to make the
following change:

.. code-block:: yaml

   - domain: "127.0.0.1.xip.io"
     ssl: null

.. note::

   SSL will not be available on Docker for Desktop clusters, as there is no public IP.
   Hence we disable SSL in the domain configuration.

Advanced: Handling multiple clusters in the same project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have multiple clusters in the same project, you will need the following
template to handle domain configuration for Docker for Desktop as well as Hasura
provisioned clusters.

.. code-block:: yaml+jinja

   {% if cluster.infra.provider == "docker-for-desktop" %}
   - domain: "127.0.0.1.xip.io"
     ssl: null
   {% else %}
   - domain: "{{ cluster.name }}.hasura-app.io"
     ssl:
       type: LetsEncrypt
       conf: {}
   {% endif %}

Step 6: Commit and push to the new cluster
------------------------------------------

Commit the files and push to the newly added Docker for Desktop cluster:

.. code-block:: bash

   git add clusters.yaml conf/domains.yaml
   git commit -m "add new docker for desktop cluster"
   git push dfd master

That's it! Your Hasura project is now deployed on the Docker for Desktop
cluster. You can see the microservices and their URLs by executing:

.. code-block:: bash

   hasura microservices list -c dfd

Tearing down
------------

You can delete all the resources Hasura created by executing the following
commands:

.. code-block:: bash
 
   kubectl delete namespace hasura
   kubectl delete configmap hasura-conf hasura-status ssh-authorized-keys
   kubectl delete secret hasura-secrets
   kubectl delete clusterrolebinding hasura-cluster-admin-binding

   # Next, delete the data directory:
   rm -r /Users/hasura-data
