.. .. meta::
   :description: API reference for Hasura's File  microservice. POST, GET and DELETE endpoints for uploading, downloading and deleting files respectively.
   :keywords: hasura, docs, File, fileStore, API reference

Separate git repository for a microservice
==========================================

By default, the source code of a microservice is inside the main hasura project git repository.

This guide describes how you can move the source code of the microservice to a different, separate git repo.

This guide assumes that you have a microservice called `www`.
and that your current directory structure is as follows:

.. code-block:: bash

   my-hasura-project
   ├── .git
   ├── hasura.yaml
   ├── clusters.yaml
   ├── conf
   ├── migrations
   └── microservices
       └── www
           ├── app/
           ├── k8s.yaml
           └── Dockerfile


At the end of these instructions this is what you will have:

.. code-block:: bash

   my-microservice
   ├── .git
   ├── app/
   └── Dockerfile

   my-hasura-project
   ├── .git
   ├── hasura.yaml
   ├── clusters.yaml
   ├── conf
   ├── migrations
   └── microservices
       └── www
           └── k8s.yaml

Note how the source-code directory ``app/`` and the ``Dockerfile`` have been removed from the project
directory and are now in a separate git repo.

Originally, you would execute a command ``git push hasura master`` to push everything to the Hasura cluster in one shot.

Now, we'll have 2 separate remotes so that the Hasura project and the microservice code can be pushed separately.

.. code-block:: bash

   $ # Push hasura configuration changes
   $ cd my-hasura-project
   $ git push hasura master


   $ # Push new source code
   $ cd my-microservice
   $ git push www master

Step 1: Separate the microservice code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a new folder called `my-microservice`.

Copy the right files over, and initialise a git repo:

.. code-block:: bash

   $ mv my-hasura-project/microservices/www/app my-microservice/
   $ mv my-hasura-project/microservices/www/Dockerfile my-microservice/
   $ cd my-microservice/
   $ git init && git add .
   $ git commit -am 'Initialises separate microservice repo'


Step 2: Add a new remote to the Hasura cluster via the conf
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The `git push` configurations are stored in the file: ``conf/ci.yaml``.

This is what your conf looks like:

.. literalinclude:: orig.ci.yaml
   :language: yaml
   :caption: conf/ci.yaml
   :linenos:
   :emphasize-lines: 21-24

This is what you need to modify it to:

.. literalinclude:: new.ci.yaml
   :language: yaml
   :caption: conf/ci.yaml
   :linenos:
   :emphasize-lines: 20-26


Notice how a new ``remote`` called ``www`` was added, and the lines describing the Dockerfile path, and the context path have been moved from the default remote, to inside the new remote.

Also, notice the changes to the Dockerfile and context path variables. These variables now reflect the path relative to the new git repo that we have created.

Apply these changes by running:

.. code-block:: bash

   $ cd my-hasura-project
   $ git add . && git commit -am 'removes microservice code'
   $ git push hasura master

As this deployment happens, you will notice that nothing gets 'built' during the docker build step of the git push.

.. code-block:: text

   remote: Creating the build directory
   remote: Checking out 'master:517d41e0f22fda5815f471a964e1feb5a799ee6a' to '/home/hasura/build/<cluster-name>'
   remote:
   remote: 0 deployment(s) found in this repo
   remote: Trying to build them...
   remote:
   remote:
   remote: Removing build directory
   remote:
   remote: Hasura build system : Tue Dec 19 16:36:23 UTC 2017: Finished build
   remote:
   remote:


Step 3: Add a remote to the microservice repo
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now, to enable git push from the microservice repo directly let's add a new git remote to the microservice.

.. code-block:: bash

   $ cd my-microservice

   $ # remember to change <cluster-name> to your actual cluster name in the command below
   $ git remote add www ssh://hasura@www.<cluster-name>.hasura-app.io:22/~/git/www

And now, push and watch your new code go live:

.. code-block:: bash

   $ git push www master
