.. .. meta::
   :description: Manual for using configuring and using custom domains on Hasura. Hasura provides free SSL certificates for your domain using LetsEncrypt.
   :keywords: hasura, docs, manual, custom domain, domain, ssl, letsencrypt

Custom domains & SSL
====================

Your Hasura cluster comes with a SSL enabled `hasura-app.io` domain. You can also point your own domain to the cluster so that your microservices/website is available on the domain.

Hasura provisions free SSL certificates for each domain you add using `LetsEncrypt <https://letsencrypt.org/>`_. All of your microservices will be available on each of these domains.

.. _adding-custom-domain:

Adding a custom domain
----------------------

- Get IP for the cluster

.. code-block:: bash

   $ ping cluster-name.hasura-app.io

- Point your domain's DNS to the cluster's IP from your registrar's dashboard by adding 2 A records for your domain pointing to the IP above

+---+----------------+---------+
| A | `*.domain.com` | 1.1.1.1 |
+---+----------------+---------+
| A | `domain.com`   | 1.1.1.1 |
+---+----------------+---------+

- Both of these entries are important for the LetsEncrypt agent to be able to generate all your SSL certificates properly

- Goto ``conf/domains.yaml`` and add the following block to the file where ``domain.com`` is your domain:

.. code-block:: yaml

   domain.com:
     ssl:
       conf:
         account: you@youremail.com
       type: LetsEncrypt


- Apply your changes to the cluster

.. code-block:: bash

   $ git add conf/domains.yaml
   $ git commit -m "added custom domain"
   $ git push hasura master # change 'hasura' to the cluster alias that you are using

Now, SSL certificates will automatically be generated and your microservices will be accessible on this domain!
