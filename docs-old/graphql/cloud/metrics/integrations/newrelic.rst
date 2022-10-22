.. meta::
   :description: New Relic Integration on Hasura Cloud
   :keywords: hasura, docs, metrics, integration, export logs, newrelic, new relic

.. _ss_newrelic_integration:

New Relic Integration
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can export metrics and operation logs of your Hasura Cloud project to `New Relic <https://newrelic.com/>`_. This can be configured 
on the integrations tab on the project's setting page.  

.. note::

  New Relic Integration is only available for Hasura Cloud projects on the ``Standard`` (pay-as-you-go) tier and above.

Configure New Relic integration
-------------------------------

Navigate to the integrations tab on project settings page to find New Relic integration.

.. thumbnail:: /img/graphql/cloud/metrics/integrate-newrelic.png
   :alt: Configure New Relic Integration
   :width: 1146px

Select the New Relic API region and enter the New Relic Insights Insert API key (follow `New Relic docs to retrieve the API key <https://docs.newrelic.com/docs/apis/get-started/intro-apis/new-relic-api-keys/#insights-insert-key>`_), host, service name and custom attributes to associate with exported logs and metrics.

.. list-table::
   :header-rows: 1
   :widths: 20 40

   * - Field
     - Description

   * - Region 
     - The region of the datacentre where your New Relic account stores its data. `Read more about regions on New Relic docs. <https://docs.newrelic.com/docs/using-new-relic/welcome-new-relic/get-started/our-eu-us-region-data-centers>`_

   * - API Key 
     - API keys are unique to your organization. An API key is required by the New Relic API to submit metrics and events to New Relic. You can get the API key from `here <https://one.newrelic.com/launcher/api-keys-ui.api-keys-launcher>`__ if you are in New Relic US region and `here <https://one.eu.newrelic.com/launcher/api-keys-ui.api-keys-launcher>`__ if you're in New Relic EU region.
   
   * - Host
     - The name of the originating host of the log and metrics.

   * - Custom Attributes
     - Custom Attributes associated with your logs and metrics. A default source tag ``hasura-cloud-metrics`` is added to all exported logs and metrics. Attributes ``project_id`` and ``project_name`` are added to all exported metrics. 

   * - Service Name 
     - The name of the application or service generating the log events.

.. thumbnail:: /img/graphql/cloud/metrics/configure-newrelic.png
   :alt: Configure New Relic Integration
   :width: 437px

After adding appropriate values, click ``Save``. 

Checking the status of the integration
--------------------------------------

The green/red dot signifies the status of the integration. Green signifies successful exporting of logs to New Relic. 
When logs are successfully exported, ``Last Exported`` is continuously updated, indicating the timestamp of the last log line successfully exported to your New Relic account.

.. thumbnail:: /img/graphql/cloud/metrics/configure-newrelic-done.png
   :alt: New Relic Integration successfully configured
   :width: 1146px

In case there is an error while exporting logs to New Relic, the dot is red and the HTTP status code of the error is displayed right below it.

.. thumbnail:: /img/graphql/cloud/metrics/configure-newrelic-fail.png
   :alt: New Relic Integration successfully configured
   :width: 1146px

View logs
---------

The logs can be viewed in your New Relic dashboard, under the ``Logs`` tab (`read more on New Relic docs <https://docs.newrelic.com/docs/logs/log-management/get-started/get-started-log-management/#find-data>`_). To navigate to the same, click ``View Logs``.

.. thumbnail:: /img/graphql/cloud/metrics/newrelic-view-logs.png
   :alt: New Relic Integration successfully configured
   :width: 1146px

.. thumbnail:: /img/graphql/cloud/metrics/newrelic-logs.png
   :alt: Logs successfully exported to New Relic
   :width: 1146px

To view only logs exported by Hasura Cloud, filter your logs using ``attributes`` you configured with this integration.

View metrics
------------

The integration exports the following five metrics to your New Relic account:

.. list-table::
   :header-rows: 1
   :widths: 30 30

   * - Metric Exported
     - Metric Name in New Relic

   * - Average number of requests
     - ``average_requests_per_minute``
  
   * - Average request execution time 
     - ``average_execution_time``

   * - Success rate of requests 
     - ``success_rate``

   * - Active subscriptions 
     - ``active_subscriptions``

   * - Number of websockets open
     - ``websockets_open``

Non zero values of all the above metrics are exported over a one minute time interval. Each metric name 
is prefixed with ``hasura_cloud``.

Graphs for all the above metrics can be viewed in your New Relic account. Under ``Browse Data`` select ``Metrics`` and choose the metrics name. To navigate to New Relic dashboard, click ``View Metrics``.

.. thumbnail:: /img/graphql/cloud/metrics/newrelic-view-metrics.png
   :alt: New Relic Integration successfully configured
   :width: 1146px

Select the graphs you want to view from the metrics explorer. 

.. thumbnail:: /img/graphql/cloud/metrics/newrerlic-metrics.png
   :alt: Metrics successfully exported to New Relic
   :width: 1146px
