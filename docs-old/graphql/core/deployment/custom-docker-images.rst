.. meta::
   :description: Requesting custom Docker images for GraphQL engine
   :keywords: hasura, docker, custom images, linux

.. _custom_docker_image:

Custom Docker images or binaries for Hasura GraphQL engine
==========================================================

.. contents:: Table of contents
   :backlinks: none
   :depth: 1
   :local:

If you need a binary for Hasura GraphQL engine to run in a non-Docker environment or you require
custom Docker images, we can make them available via a support plan. A possible use case could be that the default base image *(alpine)* is not compatible with your security scanning tool
(`Anchore <https://anchore.com/>`__, `Sysdig Secure <https://sysdig.com/products/kubernetes-security/image-scanning/>`__, etc.).

To set up such a support plan, please contact us using the form on our
`contact page <https://hasura.io/contact-us/>`__ and make sure to include the Linux flavor
(*CentOS, Debian slim, etc.*) or the target architecture for the binary you'd be
interested in.

A custom image will be made available for every stable release of GraphQL engine.
