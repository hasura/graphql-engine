Connect local kubectl to cluster
================================

When you create a new hasura cluster or add an existing one to your project, a kubectl context is created and added to your system.
To connect your local kubectl to the hasura cluster you have to set the kubectl context to the created cluster context.

.. code-block:: bash

   # Set kubectl context to cluster named alarming52
   $ kubectl config use-context alarming52
