Install Hasura CLI
==================

The Hasura CLI lets you run the console to manage your schema and define access control on your data.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Mac

      In your terminal enter the following command:

      .. code-block:: bash

        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      As this is a preview release of the Hasura CLI, once the download is complete, hit ctrl-c when you're
      prompted for your password and then move the binary manually.

      .. code-block:: bash

        mv /tmp/hasura /usr/local/bin/hasura-dev


   .. tab:: Linux

      Open your linux shell and run the following command:

      .. code-block:: bash

        curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash

      As this is a preview release of the Hasura CLI, once the download is complete, hit ctrl-c when you're
      prompted for your password and then move the binary manually.

      .. code-block:: bash

        mv /tmp/hasura /usr/local/bin/hasura-dev

   .. tab:: Windows

       Coming soon ...