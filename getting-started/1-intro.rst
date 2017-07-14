.. meta::
   :description: Part 1 of a set of learning exercises meant for exploring Hasura in detail. This pre-requisite part deals with creating a Hasura project.
   :keywords: hasura, getting started, step 1

==========================
Part I: Creating a project
==========================


Let's start by creating a project:

#. Create an account at `Hasura Project Dashboard`_
#. Click on create a project

   * Choose an infrastructure provider (*DigitalOcean or a 7 day preview project hosted by Hasura*)
   * Enter your provider's API tokens (* in the case where the free trial option is not chosen*)

#. Your project will be ready in 10-20 mins, and you'll get an email with login credentials

.. To create a project locally, (only available for OSX, Linux) refer to the :doc:`local development guide <../ref/installation/local-development>`.

.. admonition:: Video reference

   `Creating a project on DigitalOcean with free Hasura DO credits <https://youtu.be/moRHAVjoFCg>`_ (credits available on request!)

   `Introduction to the Hasura project console <https://www.youtube.com/watch?v=IIwZY1SM2dg>`_

Terminology
-----------

Project name
^^^^^^^^^^^^

Typically a word followed by an number; randomly generated when you create a project.
Eg: ``test42``. It'll be referred to as ``<project-name>``.

Project domain
^^^^^^^^^^^^^^

The domain which will resolve to your hasura project. By default, it is ``<project-name>.hasura-app.io`` or ``hasura.test`` (local).

Services
^^^^^^^^

A service is an abstract entity which captures a running web/tcp server. There are several services that come out of the box with hasura platform like ``data``, ``auth`` and ``console``.

An HTTP service, say ``svc`` is typically exposed using a subdomain at ``<svc>.<project-name>.hasura-app.io``. For example, the ``data`` service is exposed at ``data.<project-name>.hasura-app.io``.

Gateway
^^^^^^^

The Gateway is the entrypoint for your project. Every request to the project goes through the gateway and is then routed to the appropriate service. For example, all requests on ``data.<project-name>.hasura-app.io`` are forwarded to the built-in hasura provided ``data`` service.

Console
^^^^^^^

The UI service that helps you manage data, users and other services in your project. It can be accessed at ``console.<project-name>.hasura-app.io``

Admin user
^^^^^^^^^^

The ``admin`` user is a special user who can manage the project. The credentials for the ``admin`` user are sent in an email after the project creation is completed. In case of local setup, the password for the admin user is ``password``. You can use these credentials to login to the console.

.. note:: Every project gets its own ``admin`` user. `Hasura Project Dashboard`_ is the dashboard where you can create multiple projects. Login credentials that are generated for your project (and sent to you via email) have nothing to do with the login credentials of `Hasura Project Dashboard`_

Admin token
^^^^^^^^^^^

When you login to the console with the ``admin`` user credentials, you can see the admin token listed under the "Project Info" section.
