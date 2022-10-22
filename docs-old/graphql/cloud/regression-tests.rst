.. meta::
   :description: Hasura Cloud regression tests
   :keywords: hasura, docs, cloud, reliability, regression, migration

.. _regression_tests:

Regression tests
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura Cloud includes a comprehensive test bench that lets you seamlessly compile a test suite on each project, executable on any GraphQL Engine instance (ex: staging, prod).

Here's a reference development workflow that is enabled by Hasura Cloud:

#. Build your database schema and configure Hasura as required by your frontend apps or public GraphQL API.
#. Deploy changes to production after testing them.
#. Create a regression suite on production.
#. Iterate on your GraphQL schema to support new features or edits.    

   - Test changes in your dev instance against the production instance’s regression test suite. Fix any issues highlighted by the tests or plan to communicate regressions to affected stakeholders.

#. Run all changes through a CI/CD pipeline   

   - Run Regression tests programmatically for all changes in the team

#. Promote changes to prod

.. thumbnail:: /img/graphql/cloud/reliability/regression-testing-diagram.png
   :alt: Regression testing process diagram

Manage test suites
------------------

Each Hasura Cloud project can be configured with a separate test suite. Ideally, you want to create a regression test suite on an project which has received requests with operations you’d like to continue supporting or ensure are not “broken” - production or a shared QA project which receives operations in your app or, if you have a public GraphQL API, those from your consumers.

.. thumbnail:: /img/graphql/cloud/reliability/regression-tests-suites.png
   :alt: Manage regression test suites

Quick-create tests
------------------

Add important operations to your test suite with one click by adding them from your project's operation history:

.. thumbnail:: /img/graphql/cloud/reliability/regression-tests-add-operations.png
   :alt: Add tests to regression test suites

Run test suites
---------------

A good development workflow would require that tests be run 1) early in the dev process, and 2) automatically with changes, to ensure changes to the schema don't break functionality.

A test suite configured on a Hasura Cloud project can be run on the same instance or any other Hasura Cloud project registered to your team, including local ones. This is how we recommend that you incorporate regression tests into your GraphQL engine workflows:

Run regression tests manually
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Let’s say you’re a developer iterating on a feature and, as part of your work, need to modify your Postgres schema or the Hasura configuration. It is likely that you are doing so by running the console via the Hasura CLI to generate migrations that you can version control. Before committing your changes in git, you should run tests to get an early warning for potential regressions. Your team may want to designate the test suite from your production instance (or a suitable alternative) as the default suite to be used for this, and you can choose to run this test suite on your local or development instance.

.. thumbnail:: /img/graphql/cloud/reliability/regressions-run-prod-tests-on-dev.png
   :alt: Run regression tests

For example, if the column ‘title’ (in a typical authors and articles schema) has been modified as part of a feature iteration. Assuming the operation from the previous example is part of the test suite on production, here’s how the feedback on this change looks like:

.. thumbnail:: /img/graphql/cloud/reliability/regression-tests-results.png
   :alt: Regression test results

As you can see, one of the tests fails because it expects a field, title, to be part of the type articles - which is something our proposed change just modified and removed support for.

Run regression tests in CI/CD flow
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. thumbnail:: /img/graphql/cloud/reliability/regression-tests-run-cli.png
   :alt: Run regression tests via CLI

This command will fetch the entire test suite from Hasura Pro and run the tests against given endpoint using the admin secret and report the result on the terminal. The test run and the results will also be available on the Hasura Console.

You can use the Hasura Pro CLI to programmatically trigger execution of a test suite in your automated testing setup, typically in CI scripts. 

In order to communicate with Hasura’s APIs, the CLI needs to be configured with an API access token (which you can create via your Hasura Cloud settings). If you want to set the token up on a non-interactive environment, like a CI pipeline, you can obtain a token and then add to ``~/.hasura/pro_config.yaml`` with the following format:

.. code-block:: bash

   pat: <token>


View test suite results
-----------------------

.. thumbnail:: /img/graphql/cloud/reliability/regression-tests-past-runs.png
   :alt: Regression tests past results