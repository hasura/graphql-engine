.. meta::
   :description: Learn how to install Hasura - create a project by claiming a free trial project or by installing Hasura on public cloud infra or on a laptop/desktop.
   :keywords: hasura, installation, cloud, public cloud



Adding Custom APIs
==================

Sometimes, the Hasura APIs might not be enough for your backend, and you might
need to deploy your own custom APIs to handle your backend needs.

For these situations, Hasura provides both serverless functions (lambdas) through Fission, and CI
integration to quickly deploy any apps built in your favourite frameworks as
services on the Hasura platform.

Deploying your code on Hasura
-----------------------------

If you've already written your custom API  using your favourite framework, and want
to deploy it on Hasura, check out :ref:`deploy-webapp`!


Serverless Functions on Hasura
------------------------------

Serverless functions are short lived stateless functions that run on http or
event triggers. Hasura provides a simple way to set up and use serverless
functions through the Fission framework.

Serverless functions are basically used when the developer does not need to
build a full app, and just needs to run a few small functions on http calls or
other event triggers. These functions are inexpensive and efficient, and are
easy to scale under load.

To learn more about serverless functions on Hasura, check out our tutorial at
:ref:`faas-tutorial`!


