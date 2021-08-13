.. meta::
  :description: Moving existing project to a new project
  :keywords: hasura, docs, project, move, transfer, downgrade, transfer region

.. _move_project_manual:

Manually transfer existing Hasura Cloud project to a new project
================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You might want to transfer an existing Hasura cloud project to a new one in order to **transfer regions**
or to **downgrade** your project from the ``Standard`` to the ``Free`` tier.

You will have to essentially create a new Hasura project, and configure it with
the same Hasura metadata and database(s) as in the previous project.

The following is a detailed guide to achieve this. Depending on your requirements, some of the steps
in this guide might be optional for you.

.. note::

   This guide is a temporary workaround. Automated support for this will be added soon.

Step 1: Export metadata from existing project
---------------------------------------------

See :ref:`exporting metadata <exporting_metadata>` to get a copy of the current Hasura metadata
on your project.

Do ensure no further changes are made to the Hasura metadata post this.

Step 2: Create a new Cloud project
----------------------------------

See ``Step 1`` of :ref:`creating projects <create_project>` to create a new Hasura Cloud project.

Step 3: Set required ENV vars on the new project
------------------------------------------------

Set the same ENV vars as you have on the existing project to the new project.

..
  Step 4: Connect your database(s) with the same details to the new project
  -------------------------------------------------------------------------

  See ``Step 2`` of :ref:`creating projects <create_project>` to connect your existing database(s) to
  the new project. Please ensure you use the same ENV vars and set the same database names as you
  have in the existing project.

Step 4: Apply the exported metadata to the new project
------------------------------------------------------

See :ref:`applying metadata <applying_metadata>` to apply the earlier exported metadata to the new
project.

The new project should now be generating the same GraphQL API as the earlier project.

Step 5: Mark migrations as applied on the new project
-----------------------------------------------------

If you are using Hasura migrations on your project, please mark all existing migrations as applied on the
new project using the following Hasura CLI command:

.. code-block:: bash

   hasura migrate apply --skip-execution --endpoint <new-project-endpoint> --admin-secret <new-project-admin-secret> --all-databases

Step 6: Configure new project same as the earlier project
---------------------------------------------------------

Update the Hasura Cloud configuration of the new project with the same configuration as the earlier project.
i.e. custom domains, collaborators, billing, etc.

Step 7: Update GraphQL endpoint in clients consuming the API
------------------------------------------------------------

If you haven't set up a :ref:`custom domain <manage_project_domains>` for your project, you might want to update any
clients consuming the GraphQL API to point to the new project's GraphQL endpoint.

You can skip this step if you plan to rename your new project to the earlier name as suggested in the subsequent
steps. Note that this will lead to a short downtime for your clients.

Step 8: Delete the existing project
-----------------------------------

You can now delete the earlier project.

Step 9: Rename new project
--------------------------

You can now also rename your new project to the same name as the earlier project if you wish.

Zero-downtime transfer
----------------------

To achieve a zero-downtime transfer, you will need to have a :ref:`custom domain <manage_project_domains>` attached to
your Cloud project.

Once the new project is set up identically as the old one, you can update the DNS entries for your custom domain
to the new project to have a seamless transfer of traffic to the new project.

Caveats
-------

You will lose the following data from your earlier project in the process:

- all existing scheduled events
- all existing async actions
- past invocation logs of cron triggers

If you would like these to be transferred to the new project as well please get in touch with
support regarding this before deleting the old project.



