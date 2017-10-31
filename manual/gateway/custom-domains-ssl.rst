.. meta::
   :description: Manual for using configuring and using custom domains on Hasura. Hasura provides free SSL certificates for your domain using LetsEncrypt.
   :keywords: hasura, docs, manual, custom domain, domain, ssl, letsencrypt

Custom domains & SSL
====================

Your Hasura project comes with a SSL enabled `hasura-app.io` domain. You can also add your domain to the project so that your services/website is available on the domain.

Hasura provisions free SSL certificates for each domain you add using `LetsEncrypt <https://letsencrypt.org/>`_. All of your services will be available on each of these domains.

Adding a custom domain
----------------------

- Get IP for the project

.. code-block:: bash

   $ ping project-name.hasura-app.io

- Point your domain's DNS to the project's IP from your registrar's dashboard by adding a A record for your domain pointing to the IP above

+---+----------------+---------+
| A | `*.domain.com` | 1.1.1.1 |
+---+----------------+---------+

- Goto ``conf/domains.yaml`` and add the following block to the file where ``domain.com`` is your domain:

.. code-block:: yaml

   domain.com:
     ssl:
       conf:
         account: you@youremail.com
       type: LetsEncrypt


- Apply your changes to the cluster

.. code-block:: bash

   $ hasuractl cluster apply #add -c <cluster-name> in case you have multiple clusters

Now, SSL certificates will automatically be generated and your services will be accessible on this domain!
