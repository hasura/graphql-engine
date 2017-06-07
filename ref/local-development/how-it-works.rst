How to use hasuractl
====================

Creating a project
------------------

Ensure that you have a Kubernetes cluster running, and ``kubectl`` is
configured with that cluster.

For example, you can use ``minikube`` to do this.

.. code-block:: shell

  $ minikube start

Then, to create a local project, run:

.. code-block:: shell

  $ hasuractl project new --context minikube -ip $(minikube ip)

The above command takes:

* a context - which is any context defined in your kubectl config file.

* an IP address - which is the IP address of your Kubernetes cluster.

Running the above command will create a Hasura project in the Kubernetes
cluster whose context is "``minikube``" as configured in the ``kubectl`` config
file (usually at ``~/.kube/config``).

.. note::

  Even after the project is created, it will take a while for the entire
  project to be running, as it has to download few Docker images.

You can check the status of your project by using:

.. code-block:: shell

   $ kubectl get pods --namespace hasura

If the output of the above shows all pods in ``Running`` state, then your
project is ready to be used.


Accessing the project
^^^^^^^^^^^^^^^^^^^^^

Once the project is created, the **admin credentials** to access the project
are printed on the screen.

The project's console will be available at: http://console.hasura.test

Now, as the Hasura project is available at ``*.hasura.test``, you have to
configure a local DNS to resolve that address to our local Kubernetes cluster.
You can do this on your own or follow :ref:`config_dns` to set it up.

Now you can navigate to your project's console and continue working there as
would with any Hasura project.

To read on more about how to use Hasura and build your application read the
:ref:`getting_started` guide.


.. note::

  In local development, SSL creation is disabled, because it is not needed.

----

Full syntax of the command is:

.. code-block:: shell

  $ hasuractl project new --context|-c <context> --ip-address|-ip <ip address>

where,

.. list-table::

   * - ``<context>``
     - context to use from your kubectl config
   * - ``<ip address>``
     - IP address of the Kubernetes cluster, on which to install the Hasura
       project.


Updating a project
------------------

When a new Hasura platform version is released, you can update your local
project to the latest release.

If there are any breaking changes in the platform APIs, you might have to
update your custom services to support the latest version.

To update a project, run:

.. code-block:: shell

   $ hasuractl project update --context <your-context>

where ``<your-context>`` is a context of a Kubernetes cluster, configured in
your kubectl config file.

This will update your project to the latest version, and will also keep your
existing data and custom services that you created for your project. After the
update you can continue working with your project normally.

Full syntax of the command is:

.. code-block:: shell

   $ hasuractl project update --context|-c <context>

where, **context** : the context to use from your kubectl config.


Deleting a project
------------------

To delete your existing Hasura project and the Hasura services, run:

.. code-block:: shell

   $ hasuractl project delete -c <your-context>

where ``<your-context>`` is a context of a Kubernetes cluster, configured in
your kubectl config file.


This will delete your existing Hasura project and the Hasura services.

.. note::

  This will not delete your custom services or your database data. You have to
  do that manually.

----

Full syntax of the command is:

.. code-block:: shell

   $ hasuractl project delete --context|-c <context>

where, **context** : the context to use from your kubectl config.


Other commands
--------------

* ``hasuractl project version`` : Displays the current version of the Hasura
  project.
