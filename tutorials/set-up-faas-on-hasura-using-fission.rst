:orphan:

.. meta::
   :description: A tutorial on installing Fission as a Hasura service which can provide Serverless Functions
   :keywords: hasura, docs, tutorials, serverless, functions, faas, lambda, fission
   :content-tags: serverless, functions, faas

Setting up Serverless Functions (FaaS) on Hasura using Fission
=================================================================

This tutorial will guide you how to set up and use serverless functions on Hasura using the Fission framework.

`Fission <https://fission.io>`_ is a Kuberenetes Functions-as-a-Service framework wherein you can write short-lived functions and set up 
http or event triggers for them. 

This tutorial assumes that you will be setting up Fission in a cloud-based Hasura cluster (such as GKE). 

Setup
======
Requirements
~~~~~~~~~~~~~

- A Kuberenetes cluster with the Hasura platform installed
- kubectl
- python 2.7

Your kubectl should point to the cluster you want to set things up on.

Installing Fission
~~~~~~~~~~~~~~~~~~

To install Fission, run the following commands:

.. code ::

    $ kubectl create -f http://fission.io/fission.yaml
    
    $ kubectl create -f http://fission.io/fission-cloud.yaml

Install the Fission UI:

.. code ::

    $ kubectl create -f https://raw.githubusercontent.com/fission/fission-ui/master/docker/fission-ui.yaml


Setup Hasura HTTP routes
~~~~~~~~~~~~~~~~~~~~~~~~

To expose Fission and the UI as a Hasura service, you need to add http routes to the hasura-project-conf. 

The `add_routes.py <https://gist.github.com/sidmutha/530b1807204cadf41a9ca03b2cbab5a9>`_ script will do this for you.
Run the following command to do so:

.. code ::
    
    $ curl -s https://gist.githubusercontent.com/sidmutha/530b1807204cadf41a9ca03b2cbab5a9/raw/d28b6390fac374d96d0268bd35e65217e42b9623/add_routes.py | python -


This will add the following http endpoints to the hasura-project-conf:

- https://fission-ui.<project-name>.hasura-app.io : The Fission UI, accessible only to the admin
- https://fission-router.<project-name>.hasura-app.io : Fission router, to trigger functions, and is accessible to Hasura user

You are now ready to use Fission on Hasura!

Using Fission
=============
We'll manage Fission functions using the Fission UI. Head to `https://fission-ui.<project-name>.hasura-app.io/` to access the UI.

Note that this UI is accessible only if you have logged in as an admin on your Hasura project. 

To be able to create functions in Fission, you will first have to add an environment for a function to run in. 
We'll use a python environment here. 
Head to the 'Environment' page in the UI and click 'Add'. From the 'Choose Sample' dropdown, select 'Python' and click 'Save'.

You can manage functions from the 'Function' page. To add a function click on 'Add' which will take you to the create function 
page. Enter the function name, select an environment and add the function body. Click 'Deploy' to deploy the function. 
Finally, you need to add an http trigger for the function to be able to trigger it.

You can now invoke your function using `https://fission-router.<project-name>.hasura-app.io/<function>`.
The function http endpoints are accessible to the Hasura 'user' role.
