.. meta::
   :description: Project deployment regions
   :keywords: hasura, cloud, docs, regions, project, project regions

.. _project_regions:

Deployment regions
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can deploy Hasura Cloud projects in one of the supported regions (more regions are coming soon). A project's region of deployment can be selected while creating the project and it can be changed later from the project details.


Selecting the region while creating a project
---------------------------------------------

Click on the ``New Project`` button on the project-list page. You can select the project's region of deployment from the ``Select a region`` dropdown.

.. thumbnail:: /img/graphql/cloud/projects/regions-while-creation.png
   :alt: select regions while project creation


Changing region of an existing project
--------------------------------------

Go the the project details by clicking on the settings icon on your project card in the project list.

.. thumbnail:: /img/graphql/cloud/getting-started/project-manage.png
   :alt: select regions while project creation

The ``Region`` field displays the project's current region of deployment. For switching the region, click on the edit icon in the ``Region`` field.

.. thumbnail:: /img/graphql/cloud/projects/region-edit-before.png
   :alt: select regions while project creation

Choose a region of your choice and hit ``Save``.

.. thumbnail:: /img/graphql/cloud/projects/region-edit-after.png
   :alt: save regions change

There might be a short downtime while your project is being moved to a different region. If your database allows connections only from specific IP addresses, make sure you add the Hasura Cloud IP of this new region to the list of allowed IP addresses.

.. thumbnail:: /img/graphql/cloud/projects/region-edit-hasura-cloud-ip.png
   :alt: change region confirm box


.. admonition:: Note

    Support for deploying a project in multiple regions is coming soon.