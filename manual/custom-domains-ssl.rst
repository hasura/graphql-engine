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

- Point your domain's DNS to the project's IP from your registrar's dashboard
- Goto `API Gateway` on console sidebar
- Open `Domains` tab
- Type you new domain and click Add button
- Save

You can check the project details to see the status.
