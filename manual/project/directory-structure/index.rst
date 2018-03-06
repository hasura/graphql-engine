.. _hasura-project-directory-structure:

.. highlight:: bash

Project Directory Structure
===========================

The project (a.k.a. project directory) has a particular directory structure and
it has to be maintained strictly, else hasura cli would not work as expected. A
representative project is shown below:

.. toctree::

   hasura.yaml
   clusters.yaml
   conf/index
   migrations/index
   microservices/index

.. note::

  hasura cli doesn't consider any other files or directories outside of those mentioned above
