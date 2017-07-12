.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. meta::
   :description: Reference documentation for Hasura's API Gateway, the single point of contact to Hasura services for the external world, and managing custom domains.
   :keywords: hasura, docs, API Gateway, domains, custom domains, gateway, reverse proxy

Domains and API Gateway
=======================

Hasura API Gateway is the point of contact for the external world to reach a
Hasura project.

The API gateway is essentially a reverse proxy, which proxies external requests
to upstream services. It is also responsible for various other things like
session authorization, managing SSL certificates, and proxying TCP connections.

.. I have removed the section on proxying TCP connections in the Session management page. Let's revisit that call.

.. toctree::
  :maxdepth: 2

  Managing Domains
  Session management & SSL certificates
  Logs
  


