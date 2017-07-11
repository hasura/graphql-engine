.. meta::
   :description: Reference documentation for controller-conf (infra level config details for a hasura project), its Kubernetes structure and JSON object schema.
   :keywords: hasura, docs, project configuration, proj conf, configuration, controller-conf


Controller Configuration: ``controller-conf``
=============================================

This is a JSON object that contains the infrastructure level
configuration details for a hasura project. These configuration values
are used by the Hasura controller to initialise the Hasura platform, and
to keep the Hasura platform in sync with the project configuration that the
developer creates/updates (``hasura-project-conf`` and ``hasura-project-secrets``)

These settings identify what kind of environment the platform is running on.
The ``controller-conf`` does not contain application level details. The
``controller conf`` is a JSON object stored inside a kubernetes ``configmap``.


Kubernetes structure
--------------------

A ``controller-conf`` is a kubernetes ``configmap``.
Here's a sample yaml file that represents a valid ``controller-conf`` resource
for a minikube installation (for local development).

.. code-block:: yaml

   apiVersion: v1
   kind: ConfigMap
   metadata:
     name: controller-conf
     namespace: hasura
   data:
     controller-conf.json: |
       {
           "apiServer" : {
               "type": "auto"
           },
           "projectNamespace" : "default",
           "controllerNamespace" : "hasura",
           "provider" : {
               "Local": {
                   "gatewayIp" : "192.168.99.101",
                   "sshPort" : "2022",
                   "persistentDirectory" : "/data"
               }
           },
           "projectConfigMap" : "hasura-project-conf",
           "statusConfigMap" : "hasura-project-status",
           "logLevel" : "debug"
       }

JSON Object schema
------------------

The specification of the JSON object in the ``data.project`` field of the ``configmap`` is
described below:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - apiServer
     - true
     - JSON object: ``{"type":`` :ref:`ManualApiServerConf <manual-apiserver-conf>` ``}`` OR ``{"type": "auto"}``
     - The configuration details required for the controller to connect to the kubernetes API server
   * - projectNamespace
     - true
     - string
     - The kubernetes namespace where the user will create new services via Hasura, and where the
       hasura project configuration configmap lives.
       Default: ``default``
   * - controllerNamespace
     - true
     - string
     - The kubernetes namespace where the Hasura platform services live. Default: ``hasura``
   * - provider
     - true
     - :ref:`ProviderConf <provider-conf>`
     - The kubernetes namespace where the Hasura platform services live. Default: ``hasura``
   * - projectConfigMap
     - true
     - string
     - The name of the kubernetes configmap that contains the hasura-project-conf. Default: ``hasura-project-conf``
   * - statusConfigMap
     - true
     - string
     - The name of the kubernetes configmap where the controller will log progress, warnings and errors.
       Default: ``hasura-project-secrets``
   * - logLevel
     - true
     - string
     - ``DEBUG`` or ``INFO`` or ``WARN`` or ``ERROR``. Default: ``INFO``

.. _manual-apiserver-conf:

ManualApiServerConf
^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - host
     - true
     - string
     - The hostname at which the kubernetes API server is available
   * - port
     - true
     - integer
     - The port on which the kubernetes API server is available
   * - authToken
     - false
     - string
     - The Bearer token that the controller will use when contacting the kubernetes API server
   * - isHttps
     - true
     - boolean
     - Whether the kubernetes API server is available as an HTTPS endpoint to the controller

.. _provider-conf:

ProviderConf
^^^^^^^^^^^^
The ProviderConf object has one of the following schemas:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - ``Local`` or ``AzureSingle`` or ``DigitalOceanSingle``
     - true
     - :ref:`GenProviderConf <gen-provider-conf>`
     - Configuration settings for running on a minikube

.. _gen-provider-conf:

GenProviderConf
^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - gatewayIP
     - true
     - string
     - The public IP of the VM or node that the platform is running on. (Eg: ``192.168.99.101`` for minikube)
   * - sshPort
     - true
     - string
     - The port at which SSH connections to the SSH container will made; note: this is not the port for VM's sshd. (Eg: ``2022`` for minikube)
   * - persistentDirectory
     - true
     - string
     - The directory on the VM or the node where the database, filestore and other volumes are mounted from.
       (Eg: ``/data`` for minikube)
