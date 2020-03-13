.. meta::
   :description: Enable HTTPS with Hasura GraphQL engine
   :keywords: hasura, docs, deployment, https

.. _enable_https:

Enable HTTPS
============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Setting up HTTPS
----------------

Hasura GraphQL engine does not handle SSL/TLS for your API. That means, Hasura GraphQL engine cannot serve
your API on an HTTPS URL.

You should use a reverse proxy (like Nginx, Caddy,
Kong, Traefik etc.) or the cloud provider's native load balancer SSL
termination features to secure your API.

Sample configurations
---------------------

Here are a few sample configurations for some popular proxies:

`Nginx <https://nginx.org/en/docs/>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is a sample ``nginx.conf`` to proxy requests to Hasura:

.. code-block:: nginx

   server {
     listen 80;
     server_name hasura.my-domain.com;

     location / {
       proxy_pass http://localhost:8080/;
       proxy_http_version 1.1;
       proxy_set_header Upgrade $http_upgrade;
       proxy_set_header Connection "upgrade";
     }
   }

Please note that setting up SSL is not covered in this guide. You can find more
information at `Nginx docs
<https://nginx.org/en/docs/http/configuring_https_servers.html>`__.

To serve Hasura with a URL prefix instead of a separate subdomain, use
``location /hasura/`` or similar.

`Caddy <https://caddyserver.com/>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is a sample ``Caddyfile`` to proxy requests to Hasura:

.. code-block:: bash

   hasura.my-domain.com {
     proxy / http://localhost:8080
     websocket
   }

Caddy has SSL provisioning built-in with Let's Encrypt. You can find the docs at
`Caddy website <https://caddyserver.com/docs/automatic-https>`__.
   
In order to serve at a URL prefix, use the following configuration:

.. code-block:: bash

   my-domain.com {
     proxy /hasura http://localhost:8080
     websocket
     without /hasura
   }
