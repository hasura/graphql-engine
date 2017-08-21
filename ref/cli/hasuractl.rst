:orphan:

.. meta::
   :description: Reference documentation for using Hasura's command line tooling, hasuractl
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl

.. _hasuractl:

.. highlight:: bash

Hasuractl
=========

``hasuractl`` is the command line tool for the Hasura platform. 

Installation
------------

Requirements
~~~~~~~~~~~~~

* 64 bit system (Linux/Mac/Windows) and if you're using local development, at least 4GB RAM.

* Install the latest ``kubectl`` (>= 1.6.0) (https://kubernetes.io/docs/tasks/kubectl/install/)

* ``virutalbox`` for local development

* ``git-bash`` for Windows

Linux Installation
~~~~~~~~~~~~~~~~~~

Run the following command

.. code:: bash

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/latest/linux-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/


If you would like to add hasuractl manually to your path drop the ``sudo mv hasuractl /usr/local/bin`` from the above command


Windows Installation
~~~~~~~~~~~~~~~~~~~~

Download `hasuractl.exe <https://storage.googleapis.com/hasuractl/latest/windows-amd64/hasuractl.exe>`_ 
and place it in your ``PATH``. Refer to this `video <https://drive.google.com/file/d/0B_G1GgYOqazYUDJFcVhmNHE1UnM/view>`_ 
if you need help with the installation on Windows.

    In Windows, you should only use ``git-bash`` to execute commands that you see in this documentation.
    
Mac OS Installation
~~~~~~~~~~~~~~~~~~~~

Run the following command

.. code::

    $ curl -Lo hasuractl https://storage.googleapis.com/hasuractl/latest/darwin-amd64/hasuractl && chmod +x hasuractl && sudo mv hasuractl /usr/local/bin/

If you would like to add hasuractl manually to your path drop the ``sudo mv hasuractl /usr/local/bin`` from the above command

.. _working-with-hasura:

Working with a Hasura project
-----------------------------

1. Create an account at `dashboard.hasura.io <https://dashboard.hasura.io>`_ if you do not have one.

2. Log into your Hasura account:

.. code::

   $ hasuractl login

3. Set context to your Hasura project: 

   You will need to have created a Hasura project for this. You can do that
   at `dashboard.hasura.io <https://dashboard.hasura.io/projects>`_.

   Set the hasuractl context to your project context using:

.. code::

   $ hasuractl set-context <project-name>

Here <project-name> is the name of the project you created on the Hasura
Dashboard. This command also sets the kubectl context (which can be skipped using the -k flag).

Local Development
-----------------

``hasuractl`` allows you to run the Hasura platform locally on a virtual machine using ``virtualbox``. The VM is called ``minihasura``.

    Although you can develop locally on a VM, we encourage you to use a trial project which can be created on `<https://dashboard.hasura.io>`_.

Starting minihasura
~~~~~~~~~~~~~~~~~~~~~~~~

.. warning::

    Running a local VM will take upto 2 GB of RAM and will download upto 1.5 GB of docker images when run for the first time.

1. Create an account on `<https://dashboard.hasura.io>`_ and login to your account as shown in :ref:`working-with-hasura`.

2. Start the local cluster using

.. code:: bash

    $ hasuractl local start

It might take a long time for this to finish, depending on your internet connection. 
The command exits by pointing you to a url to login to the console. 
The hasuractl and kubectl contexts will be set to ``minihasura``.


Stopping minihasura
~~~~~~~~~~~~~~~~~~~

To stop the VM, run:

.. code:: bash

    $ hasuractl local stop

You can start it up again using ``hasuractl local start``.

Cleaning minihasura
~~~~~~~~~~~~~~~~~~~

You can delete Hasura specific resources using the following command:

.. code:: bash

    $ hasuractl local clean

This will only delete Hasura specific resources from the VM. All the data and configuration is deleted too. 
The underlying VM is not deleted and the downloaded docker images will still exist inside the VM. 
You can run ``hasuractl local start`` to set up Hasura again on the VM. For deleting the VM, see :ref:`local-delete`

.. _local-delete:

Deleting minihasura
~~~~~~~~~~~~~~~~~~~~

This will completely delete the minihasura VM and associated data and configurations from the system. 

.. code:: bash

    $ hasuractl local delete


Exposing a local project over the internet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Running ``hasuractl local start`` gives you a URL (e.g., c100.hasura.me) that points to your local project, 
but this URL only works locally on your computer.


If you need your iOS/Android app to access the project, or you want to share the project publicly, you need to expose the project over internet. 
To do this, login to your Hasura dashboard, go to https://dashboard.hasura.io/local-development, and modify the Public URL. 
This URL is where your project will be publicly accessible.
Now, to expose your local project, run: 

.. code:: bash

    $ hasuractl local expose

You can now access your local project at the public URL you configured earier.

.. note::

 On Windows, currently the command does not output anything when using git-bash. 
 It works nonetheless. You can use CMD instead of git-bash, **only for this command**.

Quickstart Projects
-------------------

Hasura provides readymade quickstart project templates in a variety of frameworks. 
These can help you get started with developing using these frameworks on the Hasura platform. You check out further documentation at :ref:`quickstart-cmd`.

Frequently Used Commands
-------------------------

.. _add-ssh-key-cmd:

add-ssh-key
~~~~~~~~~~~
This command picks the public key (id_rsa.pub) and adds it to the Hasura project. 
The ssh key can be generated using 

.. code::

    $ ssh-keygen -t rsa

After this run 

.. code::

    $ hasuractl add-ssh-key

to add the ssh key to your Hasura project.

.. _quickstart-cmd:

quickstart
~~~~~~~~~~
This command will initialize projects from templates from https://github.com/hasura/quickstart-docker-git into a local directory.
It will also initialize a git repository inside the directory. Along with this, if the --create flag is passed, a git-push microservice 
will also be created.

The command can be used as

.. code::

    $ hasuractl quickstart <template> <app-name> [--create] [-l|--location <path>]

**Example:**

Let's say that you want to make a nodejs express app on Hasura. You can use a template available on 
https://github.com/hasura/quickstart-docker-git to start off quickly. To simplify the process of cloning the repo, copying the 
nodejs-express into your working directory and initializing a git repo inside the directory, you can use the hasuractl quickstart command. 
If you also want to create a git-push service for the app, you can pass a --create flag to the quickstart command. The command will look like

.. code::

    $ hasuractl quickstart nodejs-express myapp --create

This will initialize a nodejs-express app in ./myapp, initialize a git repository and create a git-push microservice named 'myapp'.


To list the available templates use

.. code::

    $ hasuractl quickstart list

.. _forward-cmd:

forward
~~~~~~~
This command forwards a given port on the local machine to the given service and port in the hasura cluster.
It works in the same manner as how ssh -L does.
The format of the command is

.. code::

    $ hasuractl forward [<local-port>:<service-name>.<namespace>:<service-port>]...

This will expose <service-name>:<service-port> at localhost:<local-port>.

**Example:**

Let's say that you're working on an app and you need to access your postgres database. Normally you'll have to ssh into the 
postgres pod inside the cluster and then run psql to access the database. Using the hasuractl forward command, you can expose the 
postgres service running inside the cluster to a port on your local machine. Normally the postgres service will be running on  
port 5432 in the namespace 'hasura'. You want to access it locally on port 5432. The command to run will be

.. code:: 

    $ hasuractl forward 5432:postgres.hasura:5432

In the argument being passed to forward, 5432:postgres.hasura:5432, the first number is the local port and the last one is the port where 
the postgres service inside the hasura namespace is exposed.

Let's say you have a service 'myapp' running on port 8080 inside the cluster. Say you want to be able to access 'myapp' locally 
on port 8081. You'll want to forward your local port 8081 to port 8080 of 'myapp'. The service 'myapp' will normally be in the 
namespace 'default'. The command to forward will be

.. code:: 

    $ hasuractl forward 8081:myapp.default:8080

Finally, say you want to expose both the services above locally. Instead of running two instances of the hasuractl forward command, 
you can combine them into a single command like this

.. code:: 

    $ hasuractl forward 8081:myapp.default:8080 5432:postgres.hasura:5432

.. _set-context-cmd:

set-context
~~~~~~~~~~~
This command sets the hasuractl and kubectl context to the given project. Please note that you'll have to login to your Hasura account 
before running this command. 

.. code:: 

    $ hasuractl set-context <project-name> [-k]

Passing the -k flag will skip setting the kubectl context and will only set the hasuractl context.

get-context
~~~~~~~~~~~
This command will display the current hasuractl and kubectl contexts.

credentials
~~~~~~~~~~~
This command will display the credentials for your current Hasura project.

status
~~~~~~
This command will display the status of the current Hasura project.
