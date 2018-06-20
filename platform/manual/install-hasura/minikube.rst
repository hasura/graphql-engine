Installing Hasura on Minikube
=============================

`Minikube <https://kubernetes.io/docs/tasks/tools/install-minikube/>`_ is the
"go-to" tool by the Kubernetes community to run a single-node cluster locally as
a Virtual Machine. It is the easiest way to get started with Kubernetes without
depending on a cloud provider.

Step 1: Install Minikube
------------------------

You can install the ``minikube`` CLI by following the instructions `here
<https://kubernetes.io/docs/tasks/tools/install-minikube/>`_.

.. note::

   If this is the first time you're running a virtual machine on your
   system, you will need to enable virtualization on your BIOS and install a
   Hypervisor like KVM or VirtualBox, as mentioned in the minikube install page.

Step 2: Start Minikube cluster
--------------------------------

Once ``minikube`` is installed, you can start a local Kubernetes cluster by
executing the following command.

.. code:: bash

   minikube start

Once succeeded, the output will show the ``kube-context``, Minikube IP etc.

Step 3: Create data directory
-------------------------------

Once the minikube cluster starts running, you need to create a directory inside
the VM to store Hasura platform's data, such as Postgres, Filestore etc. You can
do this by SSH-ing to the minikube VM.

.. code:: bash

   minikube ssh

Once you're in the minikube VM, you can create a directory:

.. code:: bash

   sudo mkdir /data/hasura-data
   # (Ctrl+D to exit)

Minikube is configured to persist files stored under ``/data`` directory. (see
`Persistent Volumes
<https://kubernetes.io/docs/getting-started-guides/minikube/#persistent-volumes>`_
for reference)

Step 4: Install Hasura platform
-------------------------------

Now, you can use Hasura CLI to install Hasura platform on this cluster.

.. code-block:: bash

   hasura cluster install \
     --name=minkube-hasura-cluster \
     --provider=minikube \
     --kube-context=minikube \
     --data-path=/data/hasura-data \
     --domain=$(minikube ip).xip.io \
     --external-ip=$(minikube ip)

This could take around 5 minutes depending on your internet connection
bandwidth.

.. note::

   Hasura platform requires a domain associated with a cluster for the subdomain
   based routing to work. For minikube, we are making use of a service called
   `xip.io <http://xip.io>`_ which map domains with IP addresses in them to the given
   IP. We are getting the minikube VM's IP using ``minikube ip`` and using it in
   the domain and as external IP address for the Hasura API gateway.

Step 4: Add this cluster to a project
-------------------------------------

Clone a new Hasura project using :ref:`hasura clone <hasura_clone>` or ``cd``
into an existing project. You can then use :ref:`hasura cluster add
<hasura_cluster_add>`  command to add this minikube cluster to the project.

.. code-block:: bash

   hasura cluster add minikube-hasura-cluster \
     -c minikube \
     --kube-context=minikube

This command will add the cluster called ``minikube-hasura-cluster`` (name we used with
``--name`` flag earlier in the install command), that can be contacted using the
kube context ``minikube``, to the current project with an alias ``minikube``.

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

   - domain: "<insert-minikube-ip-here>.xip.io"
     ssl: null

You can get the minikube ip address by executing ``minikube ip``.

.. note::

   SSL will not be available on minikube clusters, as there is no public IP.
   Hence we disable SSL in the domain configuration.

Advanced: Handling multiple clusters in the same project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have multiple clusters in the same project, you will need the following
template to handle domain configuration for minikube as well as Hasura
provisioned clusters.

.. code-block:: yaml+jinja

   {% if cluster.infra.provider == "minikube" %}
   - domain: "{{ cluster.metadata.gateway.externalIPs.0 }}.xip.io"
     ssl: null
   {% else %}
   - domain: "{{ cluster.name }}.hasura-app.io"
     ssl:
       type: LetsEncrypt
       conf: {}
   {% endif %}

Step 6: Commit and push to the new cluster
------------------------------------------

Commit the files and push to the newly added minikube cluster:

.. code-block:: bash

   git add clusters.yaml conf/domains.yaml
   git commit -m "add new minikube cluster"
   git push minikube master

That's it! Your Hasura project is now deployed on the minikube cluster. You can
see the microservices and their URLs by executing:

.. code-block:: bash

   hasura microservices list -c minikube

Tearing down
------------

The easiest way to tear down is to delete your minikube cluster and create
another one when required.

.. code-block:: bash

   minikube delete

If you don't want to delete the minikube cluser, you can delete all the
resources Hasura created by executing the following commands:

.. code-block:: bash
 
   kubectl delete namespace hasura
   kubectl delete configmap hasura-conf hasura-status ssh-authorized-keys
   kubectl delete secret hasura-secrets
   kubectl delete clusterrolebinding hasura-cluster-admin-binding

   # Next, delete the data directory:

   minikube ssh
   # once inside the VM,
   sudo rm -r /data/hasura-data
   # Ctrl+D to exit
