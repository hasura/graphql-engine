.. meta::
   :description: REST connectors for actions
   :keywords: hasura, docs, action, transforms, rest connectors

.. _actions_rest_connectors:

REST Connectors for actions
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

REST Connectors for actions are used to integrate existing REST APIs to the GraphQL API without needing any middleware
or modifications to the upstream code.

REST Connectors modify the default HTTP request made by an action to adapt to your webhook's expected format by adding suitable transforms.

.. admonition:: Supported from

  REST Connectors are supported in Hasura GraphQL Engine versions ``v2.1.0`` and above

Configuring REST Connectors
---------------------------

REST Connectors can be configured either when creating a new action or editing an existing one. See the transform options :ref:`here <action_transform_types>`:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Go to the ``Actions`` tab on the console and create or modify an action. Scroll down to ``Configure REST Connectors`` section:

    .. thumbnail:: /img/graphql/core/actions/configure-rest-connectors.png
       :alt: Configure REST connectors for actions

  .. tab:: CLI

    Update the ``actions.yaml`` file inside the ``metadata`` directory and add
    a :ref:`request_transform <RequestTransformation>` field to the action:

    .. code-block:: yaml
       :emphasize-lines: 6-13

       - name: create_user
         definition:
           kind: synchronous
           handler: https://action.my_app.com/create-user
           timeout: 60
           request_transform:
             template_engine: Kriti
             method: POST
             content_type: application/json
             url: '{{$base_url}}/create_user'
             query_params:
               id: '{{$session_variables[''x-hasura-user-id'']}}'
             body: '{"username": {{$body.input.username}}}'
         comment: Custom action to create user

    Apply the metadata by running:

    .. code-block:: bash

       hasura metadata apply

  .. tab:: API

    REST Connectors can be configured for actions using the :ref:`metadata_create_action` or
    :ref:`metadata_update_action` metadata APIs by adding a
    :ref:`request_transform <RequestTransformation>` field to the args:


    .. code-block:: http
       :emphasize-lines: 24-33

       POST /v1/metadata HTTP/1.1
       Content-Type: application/json
       X-Hasura-Role: admin

       {
          "type":"create_action",
          "args":{
             "name":"create_user",
             "definition":{
                "kind":"synchronous",
                "arguments":[
                   {
                      "name":"username",
                      "type":"String!"
                   },
                   {
                      "name":"email",
                      "type":"String!"
                   }
                ],
                "output_type":"User",
                "handler":"https://action.my_app.com/create-user",
                "timeout":60,
                "request_transform": {
                  "template_engine": "Kriti",
                  "method": "POST",
                  "url": "{{$base_url}}/create_user",
                  "query_params": {
                    "id": "{{$session_variables['x-hasura-user-id']}}"
                  },
                  "content_type": "application/json",
                  "body": "{\"username\": {{$body.input.username}}}"
                }
             },
            "comment": "Custom action to create user"
          }
       }

.. _action_transform_context_variables:

Context Variables
^^^^^^^^^^^^^^^^^

You can use context variables in the transforms to achieve dynamic behavior for each request.

The context variables available in transforms are:

.. list-table::
   :header-rows: 1

   * - Context variable
     - Value

   * - $body
     - Original body of action request

   * - $base_url
     - Original configured webhook handler URL

   * - $session_variables
     - Session variables

.. _action_transforms_sample_context:

Console sample context
**********************

The console allows you to preview your transforms while configuring them. To avoid exposing sensitive information on the console UI the actual environment variables configured on the server are not
resolved while displaying the previews. Also any session variables used in the transform will not be available at the time of configuration.

Hence, the console allows you to provide mock env variables and session variables to verify your transforms. If you configure your transforms without providing the mock env/session variables
you might see a UI validation error in the preview sections.

**For example:** If your webhook handler is set as an env var as shown below then pass a mock value for that env var in the sample context:

.. thumbnail:: /img/graphql/core/actions/transform-sample-context-0.png
   :alt: Console action webhook handler
   :width: 650px

You can enter the mock env/session variables under ``Configure REST Connectors > Sample Context``:

.. thumbnail:: /img/graphql/core/actions/transform-sample-context-1.png
  :alt: Add generic sample context
  :width: 800px

.. note::

   As the sample context is only used for previews, you can still configure the transforms on the console without setting any sample context.

.. _action_transform_types:

Types of transforms
-------------------

REST Connectors allow you to add different transforms to the default HTTP request. You can also use :ref:`context variables <action_transform_context_variables>`
in the transforms to achieve dynamic behavior for each request.

You can transform your:

.. contents::
  :backlinks: none
  :depth: 1
  :local:


Request Method
^^^^^^^^^^^^^^

You can change the request method to adapt to your API's expected format.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the ``Configure REST Connectors`` section, click on ``Add Request Options Transform``:

    .. thumbnail:: /img/graphql/core/actions/transform-method.png
       :alt: Change request method
       :width: 800px

  .. tab:: CLI

    Update the ``actions.yaml`` file inside the ``metadata`` directory and add
    a :ref:`request_transform <RequestTransformation>` field to the action:

    .. code-block:: yaml
       :emphasize-lines: 8

         - name: create_user
           definition:
             kind: synchronous
             handler: https://action.my_app.com/create-user
           timeout: 60
           request_transform:
             template_engine: Kriti
             method: POST
             content_type: application/json
             url: '{{$base_url}}/create_user'
             query_params:
               id: '{{$session_variables[''x-hasura-user-id'']}}'
             body: '{"username": {{$body.input.username}}}'
         comment: Custom action to create user

    Apply the metadata by running:

    .. code-block:: bash

       hasura metadata apply

  .. tab:: API

    REST Connectors can be configured for actions using the :ref:`metadata_create_action` or
    :ref:`metadata_update_action` metadata APIs by adding a
    :ref:`request_transform <RequestTransformation>` field to the args:


    .. code-block:: http
       :emphasize-lines: 26

       POST /v1/metadata HTTP/1.1
       Content-Type: application/json
       X-Hasura-Role: admin

       {
         "type":"create_action",
         "args":{
           "name":"create_user",
           "definition":{
             "kind":"synchronous",
             "arguments":[
               {
                 "name":"username",
                 "type":"String!"
               },
               {
                 "name":"email",
                 "type":"String!"
               }
             ],
             "output_type":"User",
             "handler":"{{ACTION_BASE_URL}}",
             "timeout":60,
             "request_transform": {
               "template_engine": "Kriti",
               "method": "POST",
               "url": "{{$base_url}}/create_user",
               "query_params": {
                 "id": "{{$session_variables['x-hasura-user-id']}}"
               },
               "content_type": "application/json",
               "body": "{\"username\": {{$body.input.username}}}"
             }
           },
           "comment": "Custom action to create user"
         }
       }

Request URL
^^^^^^^^^^^

The Request URL template allows you to configure the exact API endpoint to call.

You can use the :ref:`context variables <action_transform_context_variables>` to construct the final URL.

You can also provide query params to add to the URL.

You can use the `Kriti templating language <https://github.com/hasura/kriti-lang>`__ to construct any string values here.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the ``Configure REST Connectors`` section, click on ``Add Request Options Transform``:

    .. thumbnail:: /img/graphql/core/actions/transform-url.png
       :alt: Change request URL
       :width: 800px

    The value of the final url should be reflected in the ``Preview`` section given all required :ref:`sample context <action_transforms_sample_context>` is set.

    Hit ``Save Action`` to apply your changes.

  .. tab:: CLI

    Update the ``actions.yaml`` file inside the ``metadata`` directory and add
    a :ref:`request_transform <RequestTransformation>` field to the action:

    .. code-block:: yaml
       :emphasize-lines: 10-12

         - name: create_user
           definition:
             kind: synchronous
             handler: https://action.my_app.com/create-user
           timeout: 60
           request_transform:
             template_engine: Kriti
             method: POST
             content_type: application/json
             url: '{{$base_url}}/create_user'
             query_params:
               id: '{{$session_variables[''x-hasura-user-id'']}}'
             body: '{"username": {{$body.input.username}}}'
         comment: Custom action to create user

    Apply the metadata by running:

    .. code-block:: bash

       hasura metadata apply

  .. tab:: API

    REST Connectors can be configured for actions using the :ref:`metadata_create_action` or
    :ref:`metadata_update_action` metadata APIs by adding a
    :ref:`request_transform <RequestTransformation>` field to the args:

    .. code-block:: http
       :emphasize-lines: 27-30

       POST /v1/metadata HTTP/1.1
       Content-Type: application/json
       X-Hasura-Role: admin

       {
         "type":"create_action",
         "args":{
           "name":"create_user",
           "definition":{
             "kind":"synchronous",
             "arguments":[
               {
                 "name":"username",
                 "type":"String!"
               },
               {
                 "name":"email",
                 "type":"String!"
               }
             ],
             "output_type":"User",
             "handler":"{{ACTION_BASE_URL}}",
             "timeout":60,
             "request_transform": {
               "template_engine": "Kriti",
               "method": "POST",
               "url": "{{$base_url}}/create_user",
               "query_params": {
                 "id": "{{$session_variables['x-hasura-user-id']}}"
               },
               "content_type": "application/json",
               "body": "{\"username\": {{$body.input.username}}}"
             }
           },
           "comment": "Custom action to create user"
         }
       }

.. admonition:: escapeUri

  Note that you must use the ``escapeUri`` function to urlencode templated values.
  For example, if you have to use session variables in the URL and those may contain non-ASCII values,
  then you should provide the template URL as ``{{$base_url}}/{{escapeUri $session_variables['x-hasura-user-id']}}``

Request Content-Type
^^^^^^^^^^^^^^^^^^^^

You can change the ``Content-Type`` of the request to either ``application/json`` or ``x-www-form-urlencoded``. The default is ``application/json``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Console support coming soon.

  .. tab:: CLI

    Update the ``actions.yaml`` file inside the ``metadata`` directory and add
    a :ref:`request_transform <RequestTransformation>` field to the action:

    .. code-block:: yaml
       :emphasize-lines: 9

         - name: create_user
           definition:
             kind: synchronous
             handler: https://action.my_app.com/create-user
           timeout: 60
           request_transform:
             template_engine: Kriti
             method: POST
             content_type: application/json
             url: '{{$base_url}}/create_user'
             query_params:
               id: '{{$session_variables[''x-hasura-user-id'']}}'
             body: '{"username": {{$body.input.username}}}'
         comment: Custom action to create user

    Apply the metadata by running:

    .. code-block:: bash

       hasura metadata apply

  .. tab:: API

    REST Connectors can be configured for actions using the :ref:`metadata_create_action` or
    :ref:`metadata_update_action` metadata APIs by adding a
    :ref:`request_transform <RequestTransformation>` field to the args:

    .. code-block:: http
       :emphasize-lines: 31

       POST /v1/metadata HTTP/1.1
       Content-Type: application/json
       X-Hasura-Role: admin

       {
         "type":"create_action",
         "args":{
           "name":"create_user",
           "definition":{
             "kind":"synchronous",
             "arguments":[
               {
                 "name":"username",
                 "type":"String!"
               },
               {
                 "name":"email",
                 "type":"String!"
               }
             ],
             "output_type":"User",
             "handler":"{{ACTION_BASE_URL}}",
             "timeout":60,
             "request_transform": {
               "template_engine": "Kriti",
               "method": "POST",
               "url": "{{$base_url}}/create_user",
               "query_params": {
                 "id": "{{$session_variables['x-hasura-user-id']}}"
               },
               "content_type": "application/json",
               "body": "{\"username\": {{$body.input.username}}}"
             }
           },
           "comment": "Custom action to create user"
         }
       }

With ``x-www-form-urlencoded``,  the key-value pairs in ``body`` are transformed to ``name={{$body.input.name}}&key2={{$body.input.email}}``.

Request Body
^^^^^^^^^^^^

You can generate a custom request body by configuring a template to transform the default payload to a custom payload.
The ``body`` field takes a template in the `Kriti templating language <https://github.com/hasura/kriti-lang>`__ to evaluate the transform.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the ``Configure REST Connectors`` section, click on ``Add Payload Transform``:

    A sample payload input auto-generated based on your schema is shown.

    .. thumbnail:: /img/graphql/core/actions/transform-body.png
      :alt: Add payload transformation
      :width: 1100px

    The transformed sample payload should be shown as the ``Transformed Request Body`` given all required :ref:`sample context <action_transforms_sample_context>` is set.

    Hit ``Save Action`` to apply your changes.

  .. tab:: CLI

    Update the ``actions.yaml`` file inside the ``metadata`` directory and add
    a :ref:`request_transform <RequestTransformation>` field to the action:

    .. code-block:: yaml
       :emphasize-lines: 13

         - name: create_user
           definition:
             kind: synchronous
             handler: https://action.my_app.com/create-user
           timeout: 60
           request_transform:
             template_engine: Kriti
             method: POST
             content_type: application/json
             url: '{{$base_url}}/create_user'
             query_params:
               id: '{{$session_variables[''x-hasura-user-id'']}}'
             body: '{"username": {{$body.input.username}}}'
         comment: Custom action to create user

    Apply the metadata by running:

    .. code-block:: bash

       hasura metadata apply

  .. tab:: API

    REST Connectors can be configured for actions using the :ref:`metadata_create_action` or
    :ref:`metadata_update_action` metadata APIs by adding a
    :ref:`request_transform <RequestTransformation>` field to the args:


    .. code-block:: http
       :emphasize-lines: 32

       POST /v1/metadata HTTP/1.1
       Content-Type: application/json
       X-Hasura-Role: admin

       {
         "type":"create_action",
         "args":{
           "name":"create_user",
           "definition":{
             "kind":"synchronous",
             "arguments":[
               {
                 "name":"username",
                 "type":"String!"
               },
               {
                 "name":"email",
                 "type":"String!"
               }
             ],
             "output_type":"User",
             "handler":"{{ACTION_BASE_URL}}",
             "timeout":60,
             "request_transform": {
               "template_engine": "Kriti",
               "method": "POST",
               "url": "{{$base_url}}/create_user",
               "query_params": {
                 "id": "{{$session_variables['x-hasura-user-id']}}"
               },
               "content_type": "application/json",
               "body": "{\"username\": {{$body.input.username}}}"
             }
           },
           "comment": "Custom action to create user"
         }
       }


Example
-------

Let's integrate Auth0's management API to update the profile of a user:

.. rst-class:: api_tabs
.. tabs::


  .. tab:: Console

    Go to the ``Actions`` tab on the console and create or modify an action. Scroll down to ``Configure REST Connectors`` section:

    Action definition:

    .. thumbnail:: /img/graphql/core/actions/example-transformation-0.png
       :alt: Example rest connector for actions
       :width: 1100px

    The transformation is given by:

    .. thumbnail:: /img/graphql/core/actions/example-transformation-1.png
       :alt: Example rest connector for actions
       :width: 800px

    .. thumbnail:: /img/graphql/core/actions/example-transformation-2.png
       :alt: Example rest connector for actions
       :width: 1000px

  .. tab:: CLI

    To be added

  .. tab:: API

    Action definition:

    .. code-block:: graphql

      type Mutation {
        updateProfile(picture_url : String!) : ProfileOutput
      }

      type ProfileOutput {
        id: String!
        user_metadata: String!
      }

    The transform is given by:

    .. code-block:: json

      {
        "request_transform": {
          "body": "{\"user_metadata\": {\"picture\": {{$body.input.picture_url}} } }",
          "url": "{{$base_url}}/{{$session_variables['x-hasura-user-id']}}",
          "method": "PATCH"
        }
      }
