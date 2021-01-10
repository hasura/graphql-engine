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

Enter the Datadog API key, host, service name and tags to associate with exported logs. A source tag 
``hasura-cloud-metrics`` is added to all exported logs.

.. list-table::
   :header-rows: 1
   :widths: 20 40

   * - Field
     - Description

   * - API Key 
     - API keys are unique to your organization. An API key is required by the Datadog Agent to submit metrics and events to Datadog. You can get the API key from `here <https://app.datadoghq.com/account/settings#api>`__.
   
   * - Host
     - The name of the originating host of the log.

   * - Tags
     - Tags associated with your logs.

   * - Service Name 
     - The name of the application or service generating the log events.

.. thumbnail:: /img/graphql/cloud/metrics/configure-datadog.png
   :alt: Configure Datadog Integration
   :width: 1146px

When successfully configured, ``Last Logged`` will be continuously updated as logs are exported to Datadog.

.. thumbnail:: /img/graphql/cloud/metrics/configure-datadog-done.png
   :alt: Datadog Integration successfully configured
   :width: 1146px

You can see the logs in you Datadog dashboard in the Logs tabs.

.. thumbnail:: /img/graphql/cloud/metrics/datadog-logs.png
   :alt: Logs successfully exported to Datadog
   :width: 1146px

.. note::

  Datadog allows ingestion of logs with maximum size 256kB for a single log. If a log exceeds this limit, Datadog
  will truncate the log at 256kB.
