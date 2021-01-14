.. meta::
   :description: Datadog Integration on Hasura Cloud
   :keywords: hasura, docs, metrics, integration, export logs, datadog

.. _ss_datadog_integration:

Datadog Integration
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can export metrics of you Hasura Cloud project to your organisation's Datadog dashboard. This can be configured 
on the integrations tab on the project's setting page.  

.. note::

  Datadog Integration is only available for Hasura Cloud projects on the Standard (pay-as-you-go) tier.

Configure Datadog Integration
-----------------------------
Navigate to the integrations tab on project settings page to find Datadog integration.

.. thumbnail:: /img/graphql/cloud/metrics/integrate-datadog.png
   :alt: Configure Datadog Integration
   :width: 1146px

Select the Datadog API region and enter the Datadog API key (can be retrieved by navigating to Datadog's settings page by clicking the ``Get API Key`` link), host, service name and tags to associate with exported logs. A source tag 
``hasura-cloud-metrics`` is added to all exported logs.

.. list-table::
   :header-rows: 1
   :widths: 20 40

   * - Field
     - Description

   * - Region 
     - If you are in the Datadog EU site (app.datadoghq.eu), the Region should be EU; otherwise, it should be US.

   * - API Key 
     - API keys are unique to your organization. An API key is required by the Datadog Agent to submit metrics and events to Datadog. You can get the API key from `here <https://app.datadoghq.com/account/settings#api>`__ if you are in Datadog US region and `here <https://app.datadoghq.eu/account/settings#api>`__ if you're in Datadog EU region.
   
   * - Host
     - The name of the originating host of the log.

   * - Tags
     - Tags associated with your logs.

   * - Service Name 
     - The name of the application or service generating the log events.

.. thumbnail:: /img/graphql/cloud/metrics/configure-datadog.png
   :alt: Configure Datadog Integration
   :width: 437px

After adding appropriate values, click ``Save``. 

Checking the status of the integration
--------------------------------------

The green/red dot signifies the status of the integration. Green signifies successful exporting of logs to datadog. 
When logs are successfully exported, ``Last Exported`` is continuously updated, indicating the timestamp of the last log line successfully exported to your Datadog dashboard.

.. thumbnail:: /img/graphql/cloud/metrics/configure-datadog-done.png
   :alt: Datadog Integration successfully configured
   :width: 1146px

In case there is an error while exporting logs to datadog, the dot is red and the HTTP status code of the error is displayed right below it.

.. thumbnail:: /img/graphql/cloud/metrics/configure-datadog-fail.png
   :alt: Datadog Integration successfully configured
   :width: 1146px

View logs
---------

The logs can be viewed in your Datadog dashboard, under the ``Logs`` tabs. To navigate to the same, click ``View Logs``.

.. thumbnail:: /img/graphql/cloud/metrics/datadog-view-logs.png
   :alt: Datadog Integration successfully configured
   :width: 1146px

.. thumbnail:: /img/graphql/cloud/metrics/datadog-logs.png
   :alt: Logs successfully exported to Datadog
   :width: 1146px

.. note::

  Datadog allows ingestion of logs with maximum size 256kB for a single log. If a log exceeds this limit, Datadog
  will truncate the log at 256kB.
