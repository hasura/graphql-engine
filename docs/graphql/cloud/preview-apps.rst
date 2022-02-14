.. meta::
   :description: Hasura Cloud preview apps on GitHub pull requests
   :keywords: hasura, cloud, docs, preview, review, pr, pull request, github

.. _github_preview_apps:

Hasura Cloud Preview Apps
=========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------


Hasura Cloud exposes an :ref:`API<api_ref_create_preview_app>` to create preview apps with metadata and migrations from a branch of a
GitHub repo. You can use this API for creating preview Hasura GraphQL Engine instances on pull requests and for previewing a Hasura
project with metadata and migrations from a GitHub branch. This can be done either by triggering the API directly from your CI/CD or
by using the `hasura-cloud-preview-apps <https://github.com/hasura/hasura-cloud-preview-apps>`_ GitHub action.

.. admonition:: Usage limits

  Usage is limited to 60 preview app API calls per month for users with only ``Free`` tier projects on Hasura Cloud. More plans coming soon.

Using the hasura-cloud-preview-apps GitHub action
-----------------------------------------------------

Step 1: Creating a Hasura Cloud Personal Token
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Hasura Cloud APIs can be called programmatically through a Personal Access Token. You can create a personal access token from the ``My Account``
section in the bottom left of your cloud dashboard.

.. thumbnail:: /img/graphql/cloud/preview-apps/access-token.png
   :alt: Create Access Token
   :width: 1146px

Add this personal access token in your GitHub repo secrets as ``HASURA_CLOUD_ACCESS_TOKEN``. You can do this in the ``Settings`` tab of your
GitHub repo.

.. code-block:: bash

   HASURA_CLOUD_ACCESS_TOKEN=<token>


Step 2: Add a workflow to your GitHub repo
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a file ``.github/workflows/hasura-cloud-preview-app.yml`` in your GitHub repo and add the following code:


.. code-block:: yaml

  name: 'preview-apps'
  on: # rebuild any PRs and main branch changes
    pull_request:
    push:
      branches:
        - main

  jobs:
    hasura-cloud-preview:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v2

        - name: Hasura Cloud Preview Apps
          uses: hasura/hasura-cloud-preview-apps@v0.1.4
          id: hasura_cloud_preview
          with:
            name: "repo-name-${{github.env.GITHUB_HEAD_REF}}${{github.event.number}}"
            hasuraProjectDirectoryPath: hasura
            hasuraEnv: |
              ENV_VAR_1=value_1
          env:
            GITHUB_TOKEN: ${{secrets.GITHUB_TOKEN}}
            HASURA_CLOUD_ACCESS_TOKEN: ${{secrets.HASURA_CLOUD_ACCESS_TOKEN}}

        - uses: hasura/comment-progress@v2.1.0
          with:
            github-token: ${{secrets.GITHUB_TOKEN}}
            id: preview_app_comment
            number: ${{github.event.number}}
            repository: ${{env.GITHUB_REPOSITORY}}
            message: |
              Console URL available at ${{steps.hasura_cloud_preview.outputs.consoleURL}}
              GraphQL Endpoint available at ${{steps.hasura_cloud_preview.outputs.graphQLEndpoint}}


Make sure you change the ``name``, ``hasuraProjectDirectoryPath`` and ``hasuraEnv`` fields as per your preview app requirements and repo structure.

This workflow gets triggered on all pull requests to ``main`` branch and on pushes to ``main`` branch. It does the following:

- Clones the code from the given branch

- Creates the preview app with the provided options. Refer to `the GitHub action docs <https://github.com/hasura/hasura-cloud-preview-apps>`_ for
  more options.

- Sets the following action outputs in the workflow so that you can use them for the subsequent jobs

  - ``graphQLEndpoint``: GraphQL endpoint of the created app
  - ``consoleURL``: Console URL of the created app
  - ``projectName``: Name of the created app
  - ``projectId``: The project ID of the created app

- Comments on the pull request (remove the last step if you don't want commenting)


Step 3: Database Setup
^^^^^^^^^^^^^^^^^^^^^^

If your Hasura instance uses a Postgres database, the ``hasura/hasura-cloud-preview-apps`` action can also create ephemeral databases on a
given Postgres server for use in the preview apps. This way, if you pass the connection string to your Postgres server and the associated
env vars to be exposed to Hasura, fresh databases' connection strings get added in the the given env vars.

To set this up:

1. Make sure your Postgres server accepts connections over the internet. If you don't have a Postgres server, you can follow
   `this guide <https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-20-04>`_ to set up a Postgres
   server on a Digital Ocean droplet or on a VM provided by any other vendor of your choice.

2. Add the connection URI of your Postgres server to the GitHub repo secrets as ``POSTGRES_SERVER_CONNECTION_URI``.

3. Add the following snippet above ``hasuraProjectDirectoryPath: hasura`` to your ``hasura-cloud-preview-app.yml`` file.

.. code-block:: yaml

  postgresDBConfig: |
    POSTGRES_SERVER_CONNECTION_URI=${{secrets.POSTGRES_SERVER_CONNECTION_URI}}
    PG_ENV_VARS_FOR_HASURA=PG_DB_URL_1,PG_DB_URL_2, PG_DB_URL3

The above snippet will create three temporary databases and expose their connection string to the created preview app through
``PG_DB_URL1``, ``PG_DB_URL2`` and ``PG_DB_URL3`` env vars. Edit the snippet as per the env var requirements of your Hasura instance.

.. note:: 

   If you use databases other than Postgres, you can create the ephemeral databases yourself and pass the env vars in the ``hasuraEnv`` field.

Step 4: Set up the deletion of preview apps
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a file ``.github/workflows/delete-hasura-cloud-preview-app.yml`` in your git repo and add the following code:

.. code-block:: yaml

  on:
    pull_request:
      types: [closed]

  jobs:
    delete:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v2
                
        - name: Hasura Cloud Preview Apps
          uses: hasura/hasura-cloud-preview-apps@v0.1.4
          with:
            name: "repo-name-${{github.env.GITHUB_HEAD_REF}}${{github.event.number}}"
            delete: true
          env:
            GITHUB_TOKEN: ${{secrets.GITHUB_TOKEN}} # ${{ secrets.GITHUB_TOKEN }} is provided by default by GitHub actions
            HASURA_CLOUD_ACCESS_TOKEN: ${{secrets.HASURA_CLOUD_ACCESS_TOKEN}} # Hasura Cloud access token to contact Hasura Cloud APIs

This will make sure that whenever the pull request is closed, the preview app gets deleted.

.. note:: 

   If you have used ``postgresDBConfig`` in the creation workflow, make sure that you include it in the deletion workflow as well so that the
   created databases get deleted when the pull request is gets closed/merged.

Using the API manually
----------------------

If the ``hasura/hasura-cloud-preview-apps`` GitHub action does not suit your needs, you can also directly contact the
:ref:`createGitHubPreviewApp <api_ref_create_preview_app>` API to manually
setup preview apps on your GitHub repo.

.. code-block:: graphql

   mutation createGitHubPreviewApp {
     createGitHubPreviewApp (
       payload: {
         githubPersonalAccessToken: "<github_access_token>",
         githubRepoDetails: {
             branch: "main"
             owner: "my-org"
             repo: "my-repo",
             directory: "backend/hasura"
         },
         projectOptions: {
           cloud: "aws",
           region: "us-east-2",
           plan: "cloud_free",
           name: "my-app_name"
           envVars: [{
             key: "HASURA_GRAPHQL_AUTH_HOOK",
             value: "https://my-webhook.com"
           }, {
             key: "MY_ENV_VAR_1",
             value: "my value 1"
           }]
         }
       }
     ) {
       githubPreviewAppJobID # job ID of the preview app creation job
     }
   }

This mutation queues the creation of the preview app and returns a UUID: ``githubPreviewAppJobID``. You can get the creation status of the
preview app by running the following query at ``https://data.pro.hasura.io/v1/graphql``:

.. code-block:: graphql

  query getPreviewAppCreationStatus($jobId: uuid!) {
    jobs_by_pk(id: $jobId) {
      id
      status
      tasks {
        id
        name
        task_events {
          id
          event_type
          public_event_data
          error
        }
      }
    }
  }


Make sure to set the ``githubPreviewAppJobID`` in the the ``id`` argument to the GraphQL query.

How it works?
-------------

Hasura cloud exposes the GraphQL mutations `createGitHubPreviewApp <api-reference.html#create-github-preview-app>`_ that can be triggered
programmatically using your personal access token. This mutation creates a Hasura Cloud project with the given set of environment variables
and metadata-migrations from a branch of a GitHub repo.

When the mutation is triggered, Hasura Cloud queues the given preview app for creation. If a preview app with the given name exists for a
user, it is cleaned up and metadata and migrations are applied on a fresh preview app. The cleanup is done so that every deployment is
declarative and reproducible.
