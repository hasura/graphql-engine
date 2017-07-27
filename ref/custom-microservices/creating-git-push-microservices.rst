.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. meta::
   :description: Reference documentation for securely and automatically creating a custom microservices using git push.
   :keywords: hasura, docs, custom microservices, git push, deplyment

Creating ``git push`` microservices
===================================

To deploy your code as a service using git-push you need to do the following:

#. Create a git repo with a ``Dockerfile`` in it
#. Add your SSH key to your VM via the Hasura console so that your computer is authorized to push code
#. Create a service via the Hasura console with git-push enabled

Creating a git repo
-------------------

The best way to get a base setup ready, is to grab the relevant 
base template directory from `quickstart-docker-git <https://github.com/hasura/quickstart-docker-git>`_

Clone the quickstart repository, and copy the contents of the relevant base
template directory into another folder for your project say ``myrepo``, and
initialize a git repository inside your new project folder.

.. code-block:: console

   $ git init

This is what your directory structure should now look like::

   myrepo/
      Dockerfile
      .git
      ...

Note the ``Dockerfile`` at the top level. This Dockerfile is used by the Hasura platform
automatically to build your code in the right environment.

Adding your SSH key
-------------------
Please see :ref:`add-SSH-keys` for instructions on how to create and add your SSH key to a Hasura project.

Adding a git-push enabled service
---------------------------------

In the ``Custom Microservices`` section of the Hasura console, select ``Git Push`` and create a git-push enabled service, and you're good to go.

For reference, here's a configuration screenshot:

.. rst-class:: featured-image
.. image:: ../../getting-started/gitpush.png
   :scale: 50%

Deploying a git-push enabled service
------------------------------------

Once a git-push enabled custom service has been added on the hasura console,
you must first set the hasura remote by following the instructions shown on the
manage page of your git-push service.

.. code-block:: console

   $ git remote add hasura ssh://hasura@<git-push-service-name>.<project-domain>.hasura-app.io:2022/~/git/<git-push-service-name>/

After adding the remote, you can commit your changes and push to the hasura
remote to instantly build and deploy your app in one command!

.. code-block:: console

   $ git push hasura master

Voila, your service is deployed and live! In case there are any errors in building or deploying your code,
the ``git push`` command will show you errors and the push will fail. Fix the error, and push again!

.. admonition:: Behind The Scenes

   The Hasura platform basically builds a docker image from the latest git changes
   pushed by you, and deploys the right kubernetes service, deployment underneath.

   If you want finer control over your deployment, you are encouraged to use ``kubectl``
   and peek under the hood of the service that is automatically deployed.
