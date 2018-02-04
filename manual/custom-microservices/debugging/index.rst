.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

=======================
Debugging microservices
=======================

Being able to debug and see what your microservices are doing as they're deployed is crucial to
a smooth development workflow.

Hasura makes 2 of the most common tasks easy.

1. You can :doc:`access the logs<logs>` of your service or stream them.
2. You can :doc:`"ssh" into the environment where your microservice is running<exec-container>`. At it's simplest, this means that you can open up a shell prompt (bash) in your microservice container and run commands. Technically, this is referred to as "exec"-ing a command in your container.

.. toctree::
   :maxdepth: 1
   :titlesonly:

   View logs <logs.rst>
   Exec into containers <exec-container.rst>
