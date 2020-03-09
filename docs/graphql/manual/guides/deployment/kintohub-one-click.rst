.. meta::
   :description: Deploy Hasura GraphQL engine in one-click on KintoHub
   :keywords: hasura, docs, guide, deployment, kintohub, one-click

Hasura GraphQL engine one-click app on KintoHub
===============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine is available as a one-click deployment on KintoHub. It comes with a private `Postgres <https://www.postgresql.org/>`__ instance and it is secured with a free and automatic HTTPS certificate using `Let's Encrypt <https://letsencrypt.org/>`__.

Quickstart
----------

1. Deploy your Hasura instance including Postgres database
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Click the button below to deploy your Hasura GraphQL engine and Postgres instances in your `private environment <https://docs.kintohub.com/docs/projects/environments>`__. (``Ctrl+Click`` to open in a new tab)

.. image:: https://resources.kintohub.com/deploybutton.png
   :width: 300px
   :alt: do_deploy_hasura_button
   :class: no-shadow
   :target: https://deploy.kintohub.com/hasura

The deployment will take ~1 minute.

2. Open console
~~~~~~~~~~~~~~~

Once the deployment is done (green), you can access your Hasura Console by clicking on **Copy Url** and pasting the URL in your browser.

Using migrations
----------------

For advanced use cases like using Hasura migrations, you can use our KintoHub template available on `Github <https://github.com/kintohub/hasura-template>`__ and follow this `tutorial <https://blog.kintohub.com/git-ready-with-hasura-your-team-part-2-7-d0f9617cb8f2>`__.


Adding your custom domain name
------------------------------

If you own a domain, you can map it to your Hasura instance and get a free and automatic "Let's Encrypt" certificate.

1. Scroll down to the **kintoblocks** list.
2. Click on the cog next to your **hasura** kintoblock.
3. Type your custom domain name in the **Custom Domain** section and create a CNAME in your DNS (using the url provided).
4. Click **Done Configuring**.
5. Click **Deploy**.

Wait for the deployment to be successful.
Done!

*Note: You can also check this quick video available on* `Youtube <https://www.youtube.com/watch?v=4NPgdyGqACQ>`__.

Troubleshooting
---------------

Logs (display on the top right) should be able to help you in most scenarios. If it doesn't, feel free to talk to us on `Discord <https://discordapp.com/invite/QVgqWuw>`__.
