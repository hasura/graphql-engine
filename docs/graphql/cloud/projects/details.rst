.. meta::
   :description: Project Details on Hasura Cloud
   :keywords: hasura, cloud, docs, rename, labels, details, region

.. _project_details:

Project Details
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You can view all the details about your project in the ``General`` tab.

.. thumbnail:: /img/graphql/cloud/projects/project-details.png
   :alt: General tab
   :width: 900px

Name
----

Every project has a unique name. This name is also a part of the default domain of your project. For example, if your project name is ``neat-alien-78``, the project assumes the default domain ``neat-alien-78.hasura.app``.

If you are the project owner, you can rename the project by clicking the edit icon in the ``Name`` field.

.. thumbnail:: /img/graphql/cloud/projects/project-rename-before.png
   :alt: Project rename
   :width: 900px

Once you type a name, hit ``Save``.

.. thumbnail:: /img/graphql/cloud/projects/project-rename-in-progress.png
   :alt: Project renaming
   :width: 900px

If you have added custom domain(s) to your project, you must update the DNS settings of your custom domain(s) as per the project name change.

.. thumbnail:: /img/graphql/cloud/projects/project-rename-dns-update.png
   :alt: Project rename DNS update
   :width: 900px

.. admonition:: Note

   After renaming, your project might undergo a short downtime due to the DNS resolution as per the new project name.

Region
------

The region field shows the project's region of deployment. The default region of deployment is ``AWS, us-east-2 (Ohio)``. You can changge the region by clicking hte edt icon next to it.

Pricing Plan
------------

This field shows the plan that the project is on. Read about switching pricing plans :doc:`here <pricing>`.

Hasura Cloud IP
---------------

This is the NAT gateway IP of Hasura Cloud. If your database is not exposed to the internet, you can allow connections from this IP address so that Hasura Cloud can connect to your database.

Labels
------

You can create and assign one or more labels to your projects (``dev``, ``staging``, ``production`` etc). Click on the ``+`` button in the project labels pane to assign (or create and assign) a label.

.. thumbnail:: /img/graphql/cloud/projects/project-assing-label.png
   :alt: Project labels
   :width: 900px

As of now, you can create labels, assign them to projects and remove them from projects. A user can use only the labels that they have created and they can assign labels only to the projects that they own. 

Project collaborators can only view the labels assigned to the project by the project owners.

Support for editing labels and permanently deleting them is coming soon.
