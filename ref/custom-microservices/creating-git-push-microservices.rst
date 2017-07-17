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
base template directory from `git-push-templates <https://github.com/hasura/git-push-templates>`_

This is what your directory structure should look like::

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

In the Add custom service section of the Hasura console, ensure that git-push is enabled and you're good to go.

For reference, here's a configuraiton screenshot:

.. rst-class:: featured-image
.. image:: ../../getting-started/gitpush.png
   :scale: 50%
