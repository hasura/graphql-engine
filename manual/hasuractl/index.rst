.. meta::
   :description: User's manual for using Hasura's command line tooling, hasuractl
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl

.. _hasuractl-manual:

.. highlight:: bash

hasuractl
=========

``hasuractl`` is the command line tool for the Hasura platform.

Starting v0.15, it is the primary mode of managing Hasura projects and Hasura clusters.

.. _hasuractl-installation:

Installation
------------

.. note::

   ``git`` is a dependency for ``hasuractl``. If you don't have it installed, follow the instructions `here <https://git-scm.com/book/id/v2/Getting-Started-Installing-Git>`_.

.. _hasuractl-installation-linux:

Linux
~~~~~

Install the latest ``hasuractl`` using the following command:

.. code:: bash

    $ curl -L https://hasura.io/install.sh | bash 

    # This command will download a bash script and execute it, which will in turn download the latest version of `hasuractl` and install it into `/usr/local/bin`. You will be prompted for the root password to complete installation.

.. _hasuractl-installation-windows:

Windows
~~~~~~~

Download `hasuractl.exe <https://storage.googleapis.com/hasuractl/latest/windows-amd64/hasuractl.exe>`_.
and place it in your ``PATH``. Refer to this `video <https://drive.google.com/file/d/0B_G1GgYOqazYUDJFcVhmNHE1UnM/view>`_
if you need help with the installation on Windows.

.. note::

    It is recommended to use `git-bash <https://git-scm.com/download/win>`_ on Windows to execute hasuractl commands.

.. _hasuractl-installation-macos:

Mac OS
~~~~~~

Run the following command to install ``hasuractl``:

.. code:: bash

    $ curl -L https://hasura.io/install.sh | bash 

    # This command will download a bash script and execute it, which will in turn download the latest version of `hasuractl` and install it into `/usr/local/bin`. You will be prompted for the root password to complete installation.

.. _hasuractl-getting-started:

Getting started
---------------

.. _hasuractl-getting-started-create-project:

Create a project and cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are new to ``hasuractl`` and have not created any projects or clusters before, continue. Otherwise jump to <another-location>.

1. Install ``hasuractl``
2. Login using your Hasura account:

.. code:: bash

   $ hasuractl login

3. Create a Hasura project and get a trial cluster, also change to the project directory:

.. code:: bash

   $ hasuractl create my-project --type=trial 
   $ cd my-project 

   # This command creates a directory called my-project, creates a trial cluster called `hasura` for you, adds it to the project and sets it as default.

4. Open the API console:

.. code:: bash

   $ hasuractl api-console 
          
Using the API console, you can try out Hasura APIs for Auth, Data, File and Notify. You can also create and manage tables for your database, see users in your cluster etc.

.. _hasuractl-getting-started-deploy-code:

Deploy custom code
~~~~~~~~~~~~~~~~~~

For hosting your own code or static HTML websites, Hasura provides ready-made quickstart templates for a variety of frameworks. Find all the quickstart templates `here <https://github.com/hasura/quickstart-docker-git>`_

You can add a template along with it's source code to your newly created Hasura project and deploy it to the cluster. These templates are deployed as micro-services on Hasura platform. Changes to the source code can be re-deployed using ``git push``.

1. Initialize a git repo inside your project

.. code:: bash

   $ git init 

2. Add the service and create it on the cluster:

.. code:: bash

   # for e.g., deploy a Python Flask based web server, name it api
   $ hasuractl service quickstart api --template python-flask

   # This command downloads the template, copies it into ``services`` directory in the project, creates this service on the cluster, adds a URL route for it, adds your SSH key to the cluster, creates a git remote for you to push and creates an initial commit for the code.

3. Deploy the code

.. code:: bash

   $ git push hasura master

   # Your service will be live at https://api.<cluster-name>.hasura-app.io

4. Deploy changes

Make changes to the source code in ``service/python-flask`` directory, commit them and push again:

.. code:: bash

   $ git add <files>
   $ git commit -m "<commit-message>"
   $ git push hasura master

.. note::

   You can find all the available quickstart templates here: `https://github.com/hasura/quickstart-docker-git <https://github.com/hasura/quickstart-docker-git>`_

Understanding a Hasura project
------------------------------

A *"project"* is a *"gittable"* directory in the file system, which captures all the information regarding clusters, services and migrations. It can also be used to keep source code for custom services that you write.

Creating a project
~~~~~~~~~~~~~~~~~~

.. code:: bash

   $ hasuractl create my-project 

   # creates a directory called `my-project` and initialize an empty Hasura project
   
.. note::

   You should initialize a git repo in the Hasura project directory (or add Hasura project directory to an existing git repo) so that the contents can be version controlled. You can then share this repo with others working on the same project.

Files and directories
~~~~~~~~~~~~~~~~~~~~~

The project (a.k.a. project directory) has a particular directory structure and it has to be maintained strictly, else hasuractl would not work as expected. A representative project is shown below:

.. code:: bash

   .
   ├── hasura.yaml
   ├── clusters
   │   ├── production/
   │   └── staging
   │       ├── .kubecontext
   │       ├── domains.yaml
   │       ├── gateway.yaml
   │       ├── nginx-directives.yaml
   │       ├── remotes.yaml
   │       ├── routes.yaml
   │       ├── auth.yaml
   │       ├── notify.yaml
   │       ├── filestore.yaml
   │       ├── authorized_keys 
   │       └── services
   │           ├── adminer
   │           │   ├── deployment.yaml
   │           │   └── service.yaml
   │           └── flask
   │               ├── deployment.yaml
   │               └── service.yaml
   ├── migrations
   │   ├── 1504788327_create_table_user.down.yaml
   │   ├── 1504788327_create_table_user.down.sql
   │   ├── 1504788327_create_table_user.up.yaml
   │   └── 1504788327_create_table_user.up.sql
   └── services
       ├── adminer/
       └── flask
           ├── app/
           ├── docker-config.yaml
           ├── Dockerfile
           └── README.md

* ``hasura.yaml``
  
  * Stores some metadata about the project, like name and default cluster
    
* ``clusters``
  
  * A *"cluster"* is a kubernetes cluster with Hasura platform installed on it
  * clusters directory holds information about all the clusters
  * Each sub-directory denotes a cluster added to the project
  * ``staging``
    
    * Directory that holds configuration for a cluster called staging, named after the cluster alias
    * ``.kubecontext``
      
      * Actual kubernetes context name is stored in this file
        
    * ``authorized_keys``
      
      * SSH keys allowed to access the cluster
      * One public key per line
        
    * ``*.yaml``
      
      * Configuration for the cluster, split into various yaml files
        
    * ``services``
      
      * Directory that holds kubernetes configurations for microservices added to this cluster
      * Each sub directory contains yaml spec files for a service
      * ``adminer``

        * Contains ``deployment.yaml`` and ``service.yaml`` for adminer service
 
* ``migrations``

  * Database migration files are kept in this directory
    
* ``services``

  * Default directory to store source code for custom microservices
  * Each sub-directory contains source code and *Dockerfile*
  
*hasuractl doesn't consider any other files or directories outside of those mentioned above*

Clusters
--------

A *"cluster"* is a Kubernetes system with Hasura platform installed on it. If you want to know more about how Hasura use Kubernetes, refer to our :ref:`architecture docs <platform-architecture>`.

Create a cluster
~~~~~~~~~~~~~~~~

You can create a free trial cluster using ``hasuractl`` for evaluation and development purposes. Hasura will create a virtual machine on it's cloud infrastructure, install kubernetes and Hasura on it and allot it to your Hasura account. Assuming you have already logged in and created a project called *my-project*,

.. code:: bash

   $ cd my-project
   $ hasuractl cluster create --type=trial


Add a cluster to project
~~~~~~~~~~~~~~~~~~~~~~~~

All your clusters are visible on the `Hasura Dashboard <https://dashboard.hasura.io>`_. You can add a cluster listed on your dashboard to your project by executing the following command from inside the project directory:

.. code:: bash

   $ hasuractl cluster add [cluster-name] -c [cluster-alias]

   # creates a sub-directory named `[cluster-alias]` inside the `clusters` directory, adds the cluster configuration files into it

``[cluster-name]`` is the name shown on the Hasura Dashboard and ``[cluster-alias]`` is the name you want to attach to the cluster for easier access. For example, a cluster named ``caddy89`` can be added to the project with an alias ``dev`` and you will be referring to this cluster as ``-c dev`` in all the other ``hasuractl`` commands.

.. _hasuractl-cluster-set-default:

Setting a cluster as default
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Instead of providing ``-c cluster-alias`` flag every time, you can set a cluster as the default one to contact. For example, if you want to set the cluster aliased as ``dev`` to be the default one, execute:

.. code:: bash

   $ hasuractl cluster set-default -c dev

   # adds new entry called `defaultCluster: dev` to `hasura.yaml`

.. _hasuractl-cluster-status:

Check status of a cluster
~~~~~~~~~~~~~~~~~~~~~~~~~

You can check the status of a cluster and see all the running services inside it using the status command. For ``dev`` cluster,

.. code:: bash

   $ hasuractl cluster status -c dev

   INFO Reading cluster status...                    
   INFO Status:                                      
   Cluster Name:       caddy89 
   Cluster Alias:      dev 
   Platform Version:   v0.15.2
   Cluster State:      Synced

   INFO Cluster configuration:                       
   no changes


.. note::

   If you have already set a cluster as default using :ref:`set-default <hasuractl-cluster-set-default>` command, you can omit the ``-c`` flag.

This command will show you the actual cluster name, it's alias, Hasura platform version and the cluster state.

**Cluster state** can be:

**Synced**
   All configurations have been successfully applied on the server. Current state is in sync with the server
**Applying**
   Configuration changes are being applied on the server
**Partial**
   Configurations are partially applied. Cluster is in working state, but there are some misconfigurations
**ConfigError**
   There is an error in the current configuration

.. note::

   If the state is not **Synced**, there will be an extra field called **Detail** which will tell you what went wrong so that you can fix it.

In order to see a detailed status including running services, use ``--detail`` flag.

.. code:: bash

   $ hasuractl cluster status -c dev --detail

   INFO Reading cluster status...                    
   INFO Status:                                      
   Cluster Name:       caddy89 
   Cluster Alias:      dev 
   Platform Version:   v0.15.2
   Cluster State:      Synced

   INFO Cluster configuration:                       
   no changes

   INFO Custom services:                             
   SERVICE NAME   POD NAME                 STATUS    ENDPOINT
   flask          flask-2600324275-3zsdn   Running   https://flask.caddy89.hasura-app.io
   node           node-216481375-07gxt     Running   https://node.caddy89.hasura-app.io

   INFO Hasura services:                             
   SERVICE NAME    POD NAME                         STATUS    ENDPOINT
   auth            auth-156433954-6h1lh             Running   https://auth.caddy89.hasura-app.io
   data            data-1570227353-k7smh            Running   https://data.caddy89.hasura-app.io
   filestore       filestore-3722855872-d35mj       Running   https://filestore.caddy89.hasura-app.io
   gateway         gateway-3863647987-5chjz         Running   
   le-agent        le-agent-2751747641-050kn        Running   
   notify          notify-1035130465-1lm9s          Running   https://notify.caddy89.hasura-app.io
   platform-sync   platform-sync-3459994486-swfnl   Running   
   postgres        postgres-3538737592-tnfqs        Running   
   session-redis   session-redis-1843475950-4xj0d   Running   
   sshd            sshd-289170215-5x5tb             Running   
   vahana          vahana-3833531069-jscx3          Running   


Compare cluster configurations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you make changes to a cluster's configuration by editing the files, you can see what has changed, relative to the current configuration that is present on the cluster, using the ``diff`` command. You can also us it compare the configurations of two different clusters.

.. code:: bash

   # compare current configuration on disk to that on server for a cluster called `dev`
   $ hasuractl cluster diff -c dev

   # compare configurations on server for two clusters called `dev` and `prod`
   $ hasuractl cluster diff -c dev -c prod

Apply configuration on a cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After you add a service or edit some configuration files, these changes have to be applied on a cluster. ``apply`` command comes of help here. It takes the current configuration under the ``clusters/[cluster-name]`` directory and applies (over-writes) it on the server.

.. code:: bash

   # apply configuration changes to cluster called `dev`
   $ hasuractl cluster apply -c dev

   # This command will update the configuration and also creates/updates the services in the cluster

Get credentials to access a cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you are added as a collaborator to a cluster, or when you just use a new environment, you need to have credentials so that you can access it. The usual pattern is to give the project git repository to a collaborator and also add them as collaborators to the clusters added to the project. You can read more about adding collaborators <here>.

If you are a collaborator/owner on a cluster and you have the project directory, you can get the credentials for a cluster in the project using the ``get-credentials`` command.

.. code:: bash

   # Get the credentials to access a cluster called `dev`
   $ hasuractl cluster get-credentials -c dev

   # now you can access the cluster using hasuractl

Services
--------

*Services* are containers running on Hasura platform as kubernetes objects. You can use them to run your own code, typically web-servers. You can deploy any docker container as a *Hasura Service*. You can configure HTTP routes for each service so that they can be access using the domains you configured. You can also configure continuous integration so that you can use ``git push`` to update the code for the service.

The typical workflow in deploying a service is as follows:

1. Add the service to a cluster
2. Add a route so that the service can be accessed through HTTP
3. Apply the new configuration on the cluster
4. Check cluster status to see if everything is right
5. Fix errors if there are any

If you wish to add continuous integration,

6. Add a remote for the service so that you can push your code using git
7. Apply the configuration again

.. note::

   If you want to quickly create a service from a template, write code and deploy it, take a look at the :ref:`hasuractl service quickstart <hasuractl-getting-started-deploy-code>` command.

Deploy a docker image
~~~~~~~~~~~~~~~~~~~~~

You can add docker based service to a cluster using the following commands:

.. code:: bash

   # add a service called `blog` which is using ghost docker image to a cluster called `dev`
   $ hasuractl service add blog --image=ghost:1.10 --port=2368 -c dev

   # creates kubernetes spec files required for the service inside `clusters/dev/services/blog`

   # expose this service at `blog` subdomain
   $ hasuractl route add blog --sub-domain=blog -c dev

   # adds a route entry to `clusters/dev/routes.yaml`

   # apply this configuration
   $ hasuractl cluster apply -c dev

   # check the status
   $ hasuractl cluster status -c dev --detail

   # you will find the endpoint for `blog` service here


Git push your code
~~~~~~~~~~~~~~~~~~

In order to be able to push your own code to a service, you need to add a *"remote"*. A remote is a configuration entity that defines how the docker image should be built for the corresponding service.

.. note:: It is assumed that you already have a git repo with you code and a dockerfile ready to be deployed. If not, please use :ref:`hasuractl service quickstart <hasuractl-getting-started-deploy-code>` command to get started.

.. code:: bash

   # add a service called `api` to a cluster called `dev`
   # default port is 8080 and image is hasura/hello-world
   $ hasuractl service add api -c dev

   # creates kubernetes spec files required for the service

   # expose this service at `api` subdomain
   # by default, service name is taken as the sub domain
   $ hasuractl route add api

   # adds a route entry to `clusters/dev/routes.yaml`

   # add a remote for this service
   # assuming your source code for this service is in a git repo at `/home/user/api` and has a `Dockerfile` (it can be anywhere else also)
   # path and dockerfile are provided relative to the git repo
   $ hasuractl remote add --service=api --cluster=dev --path=. --dockerfile=./Dockerfile

   # adds a remote entry to `clusters/dev/remotes.yaml`, shows the `git remote` for you to push 
   # copy the `git remote add ...` command and execute it in the repo containing service source code
   $ cd /home/user/api
   $ git remote add ...

   $ cd /home/user/hasura-project
   # apply this configuration
   $ hasuractl cluster apply -c dev

   # check the status
   $ hasuractl cluster status -c dev --detail

   # you will find the endpoint for `api` service here
   
   # now you can git push from your service source code repo to deploy to this cluster
   $ cd /home/user/api
   # make changes

   $ git add <files>
   $ git commit -m "<commit message"
   $ git push <remote-name>
   
The service will be automatically re-deployed with latest code.

Add environment variables
~~~~~~~~~~~~~~~~~~~~~~~~~

You can add environment variables to each service which will be available for the underlying docker image to use. Kubernetes spec files are directly modified for this purpose. Let's say you want to add couple of environment variables to a service called ``api`` on a cluster called ``dev``.

Edit the file ``clusters/dev/services/api/deployment.yaml`` and add the ``env`` key with required environment variables' name and values:

.. code-block:: yaml 
   :emphasize-lines: 24-30 

   apiVersion: extensions/v1beta1
   kind: Deployment
   metadata:
     creationTimestamp: null
     labels:
       app: api
       hasuraCI: "false"
       hasuraService: custom
     name: api
     namespace: default
   spec:
     replicas: 1
     strategy: {}
     template:
       metadata:
         creationTimestamp: null
         labels:
           app: api
       spec:
         containers:
         - image: hasura/hello-world:latest
           imagePullPolicy: IfNotPresent
           name: api
           env:
           - name: VERSION
             value: v2.0
           - name: ENV
             value: production
           - name: HELLO
             value: world 
           ports:
           - containerPort: 8080
             protocol: TCP
           resources: {}
   status: {}

Once you add the required variables, you can apply the new configuration on the cluster by executing:

.. code::

   $ hasuractl cluster apply -c dev

Add secret variables
~~~~~~~~~~~~~~~~~~~~

Secret variables like API token, passwords etc. are kept as kubernetes secret object. These secrets can also be made available to the docker image as environment variables. Secrets are saved directly on the cluster and are not stored locally in the cluster configuration. 

For example, if you want to add a secret token to the service ``api`` on cluster ``dev``, and consume it inside the container as an environment variable, you need to create a secret with the value.

.. code::

   # creates a kubernetes secret key called `some.token` in `hasura-cluster-secrets`
   $ hasuractl secret update some.token 9jmpkdptlm626ksw45wljokydlnf0qmu -c dev

   # you can list all the secrets that are available already
   $ hasuractl secret list -c dev

Once you add the secret key and it's value, edit the file ``clusters/dev/services/api/deployment.yaml`` and add the ``env`` key as follows:

.. code-block:: yaml
   :emphasize-lines: 24-29

   apiVersion: extensions/v1beta1
   kind: Deployment
   metadata:
     creationTimestamp: null
     labels:
       app: api
       hasuraCI: "false"
       hasuraService: custom
     name: api
     namespace: default
   spec:
     replicas: 1
     strategy: {}
     template:
       metadata:
         creationTimestamp: null
         labels:
           app: api
       spec:
         containers:
         - image: hasura/hello-world:latest
           imagePullPolicy: IfNotPresent
           name: api
           env:
           - name: TOKEN
             valueFrom:
              secretKeyRef:
                key: some.token 
                name: hasura-cluster-secrets
           ports:
           - containerPort: 8080
             protocol: TCP
           resources: {}
   status: {}

This token will now be available inside the container as an environment variable ``TOKEN``. Once you add the required variables, you can apply the new configuration on the cluster by executing:

.. code::

   $ hasuractl cluster apply -c dev

.. _hasuractl-logs:

Get logs for running services
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to get logs for a service, you need to know the name. You can use ``cluster status`` to get service names. For Hasura services, use ``--namespace=hasura``.

.. code:: bash

   # get logs for gateway service (hasura)
   $ hasuractl logs -s gateway -n hasura
   # get logs for custom service flask
   $ hasuractl logs -s flask 

.. note::

   You can also use ``--follow`` and ``--tail=<lines>`` flags to follow logs or to mention number of recent lines 

Delete a service
~~~~~~~~~~~~~~~~

A service once added to a cluster can be deleted using the following command. This will remove the service spec files, it's associated remotes and routes from the cluster configuration.

.. code:: bash

   # delete a service called `api` from cluster `dev`
   $ hasuractl service delete api -c cluster 

<incomplete>

Migrations
----------

<incomplete>

API console
-----------

<incomplete>

Set up auto-complete
--------------------



Reference
---------

.. toctree::
   :maxdepth: 1

   ref/hasuractl
