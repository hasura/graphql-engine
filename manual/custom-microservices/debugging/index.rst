.. .. meta::
   :description: How microservices work on a Hasura cluster
   :keywords: hasura, getting started, step 2

Debugging microservices
=======================

Being able to debug and see what your microservices are doing as they're deployed is crucial to
a smooth development workflow.

Hasura makes 2 of the most common tasks easy.

1. You can :doc:`access the logs <logs>` of your service or stream them.
2. You can :doc:`"ssh"<exec-container>` into the environment where your microservice is running. At it's simplest, this means that you can open up a shell prompt (bash) in your microservice container and run commands. Technically, this is referred to as "exec"-ing a command in your container.

Apart from the above, there could be scenarios where your microservice is unable to start. In those cases, since the microservice is not running, logs will not be available and you will not be able to "exec". In such a scenario,

1. Use :ref:`hasura microservice list <hasura_microservice_list>` to see the current state of the microservice.
2. Use :ref:`hasura microservice status <hasura_microservice_status>` to get a detailed status report on the microservice.


.. toctree::
   :maxdepth: 1
   :hidden:

   View logs <logs>
   Exec into containers <exec-container>

