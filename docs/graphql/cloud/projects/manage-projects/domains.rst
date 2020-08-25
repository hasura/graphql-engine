.. meta::
   :description: Managing domains on Hasura Cloud
   :keywords: hasura, docs, project, domains

.. _manage_project_domains:

Domains tab
===========

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

In the ``Domains`` tab, you can see the default Hasura domain, and you have the possibility to add custom domains.

Adding a custom domain
----------------------

You can add a custom domain to your Hasura Cloud project by following the steps below.

Step 1: Navigate to add a custom domain
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Domains`` tab, click on the ``New Custom Domain`` button.

.. thumbnail:: /img/graphql/cloud/projects/add-custom-domain.png
   :alt: Add custom domain
   :width: 727px

Step 2: Add your custom domain
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Enter your custom domain and click the ``Add`` button.

.. thumbnail:: /img/graphql/cloud/projects/choose-custom-domain.png
   :alt: Choose custom domain
   :width: 727px

Step 3: Add the record to your DNS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After adding a custom domain, the following window will show up:

.. thumbnail:: /img/graphql/cloud/projects/dns-settings.png
   :alt: DNS settings
   :width: 727px

If you haven't already done so, add the default Hasura domain as a ``CNAME`` record to your DNS.

Until this is done, the dashboard will show a notice that the DNS validation is pending. 

.. thumbnail:: /img/graphql/cloud/projects/dns-validation-pending.png
   :alt: DNS validation pending
   :width: 727px

.. note::

  Depending on your DNS provider, it might take up to 24 hours for the DNS record to be added.

DNS validated
-------------

Once the DNS is validated, the dashboard will update the status with the following notice:

.. thumbnail:: /img/graphql/cloud/projects/dns-validated.png
   :alt: DNS validated
   :width: 727px
