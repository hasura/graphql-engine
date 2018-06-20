Getting Started
===============

Hasura GraphQL engine lets you setup your own GraphQL server in minutes. To get started, install Hasura CLI from the options below.

.. note::

  What is Hasura GraphQL engine? 
  Watch this 3 minute video to know more. if you have any queries kindly ping us on chat.

Install Hasura CLI
******************

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash
      
        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      Once the download is complete, hit ctrl-c before you're prompted for your password and then move the file manually. We're doing this because this is a preview release of the hasura CLI!

      .. code-block:: bash
      
        mv /tmp/hasura /usr/local/bin/hasura-dev


   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash
      
        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      Once the download is complete, hit ctrl-c before you're prompted for your password and then move the file manually. We're doing this because this is a preview release of the hasura CLI!

      .. code-block:: bash
      
        mv /tmp/hasura /usr/local/bin/hasura-dev

   .. tab:: Windows


Installation
************

You can install Hasura GraphQL engine on Heroku or on your local machine using Docker. The easiest way to get the engine up and running is on Heroku free tier.

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
    :alt: heroku_deploy_button
    :target: https://heroku.com/deploy?template=https://github.com/karthikvt26/heroku-push

If you want a custom installation, please checkout :ref:`custom_graphql_installation`.

.. toctree::
  :hidden:

  install
  work-with-existing-schema
