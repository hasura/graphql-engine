Installing Hasura on any Kubernetes Cluster
===========================================

Installing Hasura on any Kubernetes cluster is as easy as writing the
configuration required in a yaml file and using it to execute :ref:`hasura
cluster install <hasura_cluster_install>`.

Step 1: Write the configuration
-------------------------------

Here is an example for a cluster configuration (particularly on GKE).

.. literalinclude:: custom-cluster.yaml
   :language: yaml
   :caption: custom-cluster.yaml
   :linenos:

Here's an explanation for all the fields in this file:

``name``
^^^^^^^^

Name of the cluster. This name is important as a git remote is created on the
cluster with this name, to enable git-pushes. Before the configuration is
applied, a remote has to be present on the cluster.

``alias``
^^^^^^^^^

This is used only when the cluster is added to a project. Alias is used for
referring to the cluster later with CLI commands.

``data``
^^^^^^^^

Arbitrary JSON objects can be inserted here and can be later used in templating
the configuration (present in ``conf/`` directory. Only used after cluster is
added to a project.

``kubeContext``
^^^^^^^^^^^^^^^

The Kubernetes context used to contact the cluster while installing and later
after adding to a project. The context should exist in the user's machine.

``config``
^^^^^^^^^^

Tells the Hasura CLI where it can find (or where it should create while
installing) the cluster's configuration, including infrastructure and metadata.
This information is vital for Hasura CLI to talk to a Kubernetes cluster.

- ``namespace``

Namespace where the configuration can be found.

- ``configmap``

Name of the configmap where configuration is stored.

``infra``
^^^^^^^^^

Infrastructure identifier for this cluster. Since we are installing Hasura on
the cluster manually, we set this to ``provider: custrom`` (mandatory).

``metadata``
^^^^^^^^^^^^

Metadata about the cluster and how the cluster should behave. Only used while
installing. Later stored in the configmap mentioned above.

- ``namespaces``

Indicates the namespaces to be used by the Hasura platform.

``hasura`` indicates name of the namespace in which platform components should
be created. This namespace should not exist already.

``user`` indicates the namespace where all user components, like conf, secrets,
microservices etc. should be created. This namespace should already exist.

- ``registry``

Docker registry to push images built on the cluster. In a multi-node
environment, this is required for a smooth working. ``dockercfgSecret`` is the
name of Kubernetes secret object to be used. ``prefix`` indicates the registry
url. For a docker hub repo, this will be ``docker.io/<username>``.

- ``gateway``

`Kubernetes ServiceSpec
<https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/#servicespec-v1-core>`_
to be used for Hausra API Gateway. The port configuration should not be changed,
apart from the ``port`` values. ``selector`` should also be left as is (``app:
gateway``).

``type`` can be set depending on the environment.

- ``postgres``

``volume`` indicates the `Kubernetes Volume
<https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/#volume-v1-core>`_
to be used for storing Hasura Postgres data. 

- ``filestore``

``volume`` indicates the `Kubernetes Volume
<https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/#volume-v1-core>`_
to be used for storing Hasura Filestore data.

- ``sessionStore``

``volume`` indicates the `Kubernetes Volume
<https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.10/#volume-v1-core>`_
to be used for storing Hasura Redis data.

If disks like GCEPersistentDisk or AWSElasticBlockStorage are used, they should
already be created. Hasura makes no attempt in creating these disks. If
Persistent Volume Claims are used, the user should make sure that required PVC
objects and storage classes are present.

.. note::

   Since PVC objects are namespaced, creating them before installing Hasura is
   not possible, as ``hasura`` namespace will not be available. So, if PVC is
   being used for volumes, add their names in the cluster spec, but only create
   the PVC objects after executing the :ref:`hasura cluster install
   <hasura_cluster_install>` command and when the command waits at
   ``Initializing the Hasura platform``.

Step 2: Create required resources
---------------------------------

Create all the required resources including the Kubernetes cluster, disk and
static IPs. Make sure that the names are as it is written in the yaml file. Also
ensure that the given ``kubeContext`` is pointing to the desired cluster.

Step 3: Setup a domain for the cluster
--------------------------------------

If you already have a static IP address created for the cluster, associate a
domain with that. If you are using a dynamic cloud provider provisioned load
balancer (``type: LoadBalancer``), you can use any random domain (like
``example.com``) while creating the cluster and later change it.

.. note::

   If you don't own a domain and still want to get a domain for an IP address, you
   can use `xip.io <http://xip.io>`_. Any IP address given as subdomain to
   xip.io will map to the IP address. For e.g. ``101.202.111.222.xip.io``
   resolves to ``101.202.111.222``.

   Note that SSL won't be available for these domains as LetsEncrypt rate limits
   will be hit.

Step 4: Install Hasura platform
-------------------------------

Now, you can use Hasura CLI to install Hasura platform on this cluster.

.. code-block:: bash

   hasura cluster install \
     --file custom-cluster.yaml \
     --domain=<your-domain>

This could take around 5 minutes depending on your internet connection
bandwidth.

Step 5: Add this cluster to a project
-------------------------------------

Clone a new Hasura project using :ref:`hasura clone <hasura_clone>` or ``cd``
into an existing project. You can then use :ref:`hasura cluster add
<hasura_cluster_add>`  command to add this cluster to the project.

.. code-block:: bash

   hasura cluster add --file custom-cluster.yaml

This command will add the cluster defined in ``custom-cluster.yaml`` to your
project. The name, alias and all other configuration will be taken from the yaml
file.

Step 6: Configure domains in the project
----------------------------------------

Your current Hasura project is most likely to have the domain configured as ``"{{
cluster.name }}.hasura-app.io"`` in :ref:`conf/domains.yaml
<hasura-dir-conf-domains.yaml>`. This domain will only work for clusters
provisioned through Hasura, not for user provisioned ones. Hence, you need to
edit this file and change the domain configuration.

Edit :ref:`conf/domains.yaml <hasura-dir-conf-domains.yaml>` to make the
following change:

.. code-block:: yaml

   - domain: "<insert-your-domain-here>"
     ssl:
       type: LetsEncrypt
       conf: {}

.. note::

   If you're using ``xip.io`` domain, use ``ssl: null`` as certificates cannot
   be issued for this domain.

Advanced: Handling multiple clusters in the same project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have multiple clusters in the same project, you will need the following
template to handle domain configuration for this cluster as well as Hasura
provisioned clusters.

.. code-block:: yaml

   {% if cluster.infra.provider == "custom" %}
   - domain: "<insert-your-domain-here>"
     ssl: null
   {% else %}
   - domain: "{{ cluster.name }}.hasura-app.io"
     ssl:
       type: LetsEncrypt
       conf: {}
   {% endif %}


.. note::

   You can also use conditionals like ``cluster.name == "my-custom-cluster"`` in
   the if-clauses.

   If you want to add more conditions, you can make use of ``{% elif <condition>
   %}`` statements.

   More documentation on the templates are available at :ref:`conf-templating`.

Step 7: Commit and push to the new cluster
------------------------------------------

Commit the files and push to the newly added minikube cluster:

.. code-block:: bash

   git add clusters.yaml conf/domains.yaml
   git commit -m "add new custom cluster"
   git push minikube master

That's it! Your Hasura project is now deployed on the cluster. You can
see the microservices and their URLs by executing:

.. code-block:: bash

   hasura microservices list -c <cluster-alias>

Tearing down
------------

You can delete all the resources Hasura created by executing the following
commands:

.. code-block:: bash
 
   kubectl delete namespace hasura
   kubectl delete configmap hasura-conf hasura-status ssh-authorized-keys
   kubectl delete secret hasura-secrets
   kubectl delete clusterrolebinding hasura-cluster-admin-binding

   # Next, delete all the infrastructure components like disks, ip address etc.
