.. meta::
   :description: Reference documentation for hasura-project-status, the status of implementing the latest project conf, its Kubernetes structure and JSON object schema.
   :keywords: hasura, docs, project configuration, proj conf, configuration, hasura-project-secrets

Project Status: ``hasura-project-status``
=========================================

This JSON object represents the status of processing the latest available ``hasura-project-conf``
and is used to note errors or warnings emitted while applying the configuration. This
JSON object is also updated in a kubernetes ``configmap``.

.. note:: To fetch the project status on the cluster use the
  following kubectl command:

  ``kubectl get cm hasura-project-status``


Kubernetes structure
--------------------

``hasura-project-status`` is a kubernetes ``configmap``. It is generated automatically by
the hasura controller and should never be created/updated/deleted manually.
Here's a sample condensed yaml file that represents a generated ``hasura-project-status`` resource.

.. code-block:: yaml

   apiVersion: v1
   data:
     status_version: v1
     state: |
       {
         "summary":{...},
         "platformVersion":"c40e5ba-dirty-7139f37c",
         "detail":{
           "initPods":{...},
           "processedResources":{...}
         }
       }
     services: |
      {
         "summary":{...},
         "platformVersion":"0.8.1",
         "detail":{
            "componentsState":{...},
            "processedResources":{...}
          }
      }
   kind: ConfigMap
   metadata:
     name: hasura-project-status
     namespace: default

``state`` JSON schema
---------------------

The specification of the JSON object in the ``data.state`` field of the ``configmap`` is
described below:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - platformVersion
     - true
     - string
     - A string representing a platform version number. semver type value for releases, commit hashes for nightly builds.
   * - summary
     - true
     - :ref:`StateSummaryStatus <state-summary-status>`
     - An object representing a summary of the state processing status
   * - detail
     - true
     - :ref:`StateDetailStatus <state-detail-status>`
     - Details of the platform state initialisation performed by the controller

.. _state-detail-status:

StateDetailStatus
^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - initPods
     - true
     - :ref:`StateInitPodsStatus <state-initpods-status>`
     - Details of the kubernetes pods were run for initialisation
   * - processedResources
     - true
     - :ref:`ProcessedResources <processed-resources>`
     - References to what configuration resources were processed to initialise the state

.. _state-initpods-status:

StateInitPodsStatus
^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - postgres
     - true
     - :ref:`ResourceDetail <resource-detail>`
     - Details of the kubernetes pod used to initialise postgres
   * - data
     - true
     - :ref:`ResourceDetail <resource-detail>`
     - Details of the kubernetes pod used to initialise the Hasura data API service
   * - auth
     - true
     - :ref:`ResourceDetail <resource-detail>`
     - Details of the kubernetes pod used to initialise the Hasura auth API service
   * - filestore
     - true
     - :ref:`ResourceDetail <resource-detail>`
     - Details of the kubernetes pod used to initialise hasura filestore API service
   * - console
     - true
     - :ref:`ResourceDetail <resource-detail>`
     - Details of the kubernetes pod used to initialise the console (creating hasura data API metadata so that the auth and filestore tables are API-fied)

.. _processed-resources:

ProcessedResources
^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - configMaps
     - true
     - :ref:`ResourcesObject <resources-object>`
     - Map (object) of the ``configmap`` resources that have been procssed
   * - secrets
     - true
     - :ref:`ResourcesObject <resources-object>`
     - Map (object) of the ``secret`` resources that have been procssed
   * - services
     - true
     - :ref:`ResourcesObject <resources-object>`
     - Map (object) of the ``service`` resources that have been procssed

.. _resource-detail:

ResourceDetail
^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - name
     - true
     - string
     - The name of the kubernetes resource (Eg: ``pods.hasura.postgres-init-0.8.1``)
   * - uid
     - true
     - string
     - The kubernetes given uid of the kubernetes resource (Eg: ``e354671b-2690-11e7-8187-08002788ba09``)
   * - state
     - true
     - string
     - The kubernetes given state of the kubernetes resource ``Pending`` or ``Running`` or ``Partial`` or ``Succeeded`` or ``Error``

.. _resources-object:

ResourcesObject
^^^^^^^^^^^^^^^
This is a map of the kubernetes resource objects and their resource versions
used.

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - <kubernetes-resource-name> (eg: ``default.hasura-project-conf``)
     - false
     - string
     - The kubernetes resource version (eg: ``87802``)

.. _state-summary-status:

StateSummaryStatus
^^^^^^^^^^^^^^^^^^
One of the following:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``Initialising``
     - Ongoing initialisation process

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``Initialised``
     - Successfully completed initialised process

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``ConfigError``
     - Configuration error detected during initialisation
   * - data
     - true
     - :ref:`ConfigError <config-error-status>`
     - Details of the configuration error

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``InternalError``
     - Internal or unexpected error during initialisation process
   * - data
     - true
     - :ref:`InternalError <internal-error-status>`
     - Details of the internal error

.. _config-error-status:

ConfigError
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - path
     - false
     - string
     - The JSON path of JSON object key where the configuration error was noticed
   * - detail
     - false
     - string
     - The error thrown by the controller

.. _internal-error-status:

InternalError
^^^^^^^^^^^^^

This error object is used to denote an unexpected state or
assertion failures.

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - context
     - false
     - string
     - Short form error message
   * - detail
     - true
     - string
     - Long form error message

``services`` JSON schema
------------------------

The specification of the JSON object in the ``data.services`` field of the ``configmap`` is
described below:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - platformVersion
     - true
     - string
     - A string representing a platform version number. semver type value for releases, commit hashes for nightly builds.
   * - summary
     - true
     - :ref:`ServicesSummaryStatus <services-summary-status>`
     - An object representing a summary of the state processing status
   * - detail
     - true
     - :ref:`ServicesDetailStatus <services-detail-status>`
     - Details of the latest configuration applied by the controller

.. _services-summary-status:

ServicesSummaryStatus
^^^^^^^^^^^^^^^^^^^^^
One of the following:

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``Applying``
     - Ongoing configuration application process

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``Synced``
     - Successfully completed applying configuration

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``Partial``
     - Configuration not applied completely; warnings will be emitted and available in the ``services.details`` value

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``ConfigError``
     - Configuration error detected during initialisation
   * - data
     - true
     - :ref:`ConfigError <config-error-status>`
     - Details of the configuration error

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - tag
     - true
     - string ``InternalError``
     - Internal or unexpected error during initialisation process
   * - data
     - true
     - :ref:`InternalError <internal-error-status>`
     - Details of the internal error


.. _services-detail-status:

ServicesDetailStatus
^^^^^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - componentsState
     - true
     - :ref:`ComponentsStatus <components-status>`
     - Internal or unexpected error during initialisation process
   * - data
     - true
     - :ref:`ProcessedResources<processed-resources>`
     - References to what configuration resources were processed during the
       application of the latest configuration

.. _components-status:

ComponentsStatus
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - state
     - true
     - ``[`` :ref:`ResourceDetail <resources-detail>` ``]``
     - Array of ``ResourceDetail`` objects that represent the application state (postgres)
   * - gateway
     - true
     - :ref:`GatewayDetail <gateway-detail>`
     - Information about how gateway configuration was processed and applied
   * - services
     - true
     - ``[`` :ref:`ResourceDetail <resource-detail>` ``]``
     - Array of ``ResourceDetail`` objects that represent all the Hasura platform services
   * - sshd
     - true
     - ``[`` :ref:`ResourceDetail <resource-detail>` ``]``
     - Array of ``ResourceDetail`` objects that constitute the SSH service (deployment, configmap)


.. _gateway-detail:

GatewayDetail
^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - warnings
     - true
     - :ref:`GatewayWarnings<gateway-warnings>`
     - Lists of warnings obtained while processing/applying the gateway configuration and 
   * - resources
     - true
     - ``[`` :ref:`ResourceDetail <resource-detail>` ``]``
     - Array of ``ResourceDetail`` objects that constitute the gateway service of the Hasura platform (deployment + configmap)

.. _gateway-warnings:

GatewayWarnings
^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Always present
     - Schema
     - Description
   * - server
     - true
     - ``[string]``
     - Lists of warnings emitted by server_block configuration rules
   * - location
     - true
     - ``[string]``
     - Lists of warnings emitted by location_block configuration rules

