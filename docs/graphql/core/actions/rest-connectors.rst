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

REST Connectors for actions are used to integrate existing REST APIs as GraphQL APIs
without changing any upstream code.

REST Connectors work by changing the default HTTP request made by an action to a
different HTTP request by adding suitable transforms.

.. admonition:: Supported from

  REST Connectors are supported in Hasura GraphQL Engine versions ``v2.1.0`` and above

Configuring REST Connectors
---------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Go to the ``Actions`` tab on the console and create or modify an action. Scroll down to ``Configure REST Connectors`` section:

    .. thumbnail:: /img/graphql/core/actions/configure-rest-connectors.png
       :alt: Configure REST connectors for actions

  .. tab:: CLI

    Will be added soon

  .. tab:: API

    REST Connectors can be added to actions using the :ref:`create_action metadata API <metadata_create_action>` or
    :ref:`update_action metadata API <metadata_update_action>` by adding a
    :ref:`request_transform <RequestTransformation>` field to the args:


    .. code-block:: http
       :emphasize-lines: 24-26

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
                   "body": "{{$body.input.name}}"
                },
             },
            "comment": "Custom action to create user"
          }
       }

Types of transforms
-------------------

REST Connectors allows you to add different types of "transforms" to the default HTTP request.
You can also use "context variables" in the transforms to achieve dynamic behaviour for each request.

Context Variables
*****************

The context variables available in transforms are:

.. list-table::
   :header-rows: 1

   * - Context variable
     - Value

   * - $body
     - Original body of action request

   * - $base_url
     - Original configured URL

   * - $session_variables
     - Session variables

Sample Context
~~~~~~~~~~~~~~

The console allows you to provide mock ``session variables`` and ``env variables`` to test your transforms.

Configure an ``env var`` in the action webhook handler.

.. thumbnail:: /img/graphql/core/actions/transformation-context-vars-0.png
   :alt: Console action webhook handler
   :width: 800px

Add the ``env var`` value to the ``Sample Context`` under ``Sample Env Variables``.

.. thumbnail:: /img/graphql/core/actions/transformation-context-vars-1.png
   :alt: Console action context env
   :width: 800px

The value should be reflected in the ``{{$base_url}}`` in ``Change Request Options`` section:

.. thumbnail:: /img/graphql/core/actions/transformation-context-vars-2.png
   :alt: Console action req options transformation
   :width: 800px

``Session vars`` can also be added to the ``Sample context`` as shown above,
and used like so: ``{{$session_variables['x-hasura-user-id']}}``.
The above screen also shows an example of using the session vars from context.

.. admonition:: Context variables validation error

  Actual environment variables are not resolved during testing transforms as it could expose sensitive information to the UI.
  
  Note that if you don't provide mock ``env/session variables`` and test your transform, you would get a UI validation error.
  Considering this section is only used for testing, ``Create Action`` button will still be usable.
  When you click on ``Create Action``, any referenced envs are validated at the server without leaking any sensitive information to the UI.


Request body
************

Generate a request body by configuring a template to transform the default payload to a custom payload.
The ``body`` field takes a template in the `Kriti templating language <https://github.com/hasura/kriti-lang>`__ to evaluate the transform.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the ``Configure REST Connectors`` section, click on ``Add Payload Transform``:

    .. thumbnail:: /img/graphql/core/actions/payload-transform.png
       :alt: Add payload transformation
       :width: 1100px

  .. tab:: CLI

    Will be added soon

  .. tab:: API

    .. code-block:: json
      :emphasize-lines: 3

      {
        "request_transform": {
           "body": "{\n  \"users\": {\n    \"name\": {{$body.input.arg1.username}},\n    \"password\": {{$body.input.arg1.password}}\n  }\n}",
        }
      }

Content Type
************

You can change the ``Content-Type`` of the request to either ``application/json`` or ``x-www-form-urlencoded``. The default is ``application/json``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Console support coming soon.

  .. tab:: CLI

    Will be added soon

  .. tab:: API

    .. code-block:: json
      :emphasize-lines: 7

      {
        "request_transform": {
           "body": {
               "name": "{{$body.input.name}}",
               "email": "{{$body.input.email}}",
           },
           "content_type": "x-www-form-urlencoded"
        }
      }

With ``x-www-form-urlencoded``,  the key-value pairs in ``body`` are transformed to ``name={{$body.input.name}}&key2={{$body.input.email}}``.

URL
***

Transform the request URL. This can be used to embed, say user-id, in the url path.
You can also provide ``query_params`` to add to the URL.
You can use the `Kriti templating language <https://github.com/hasura/kriti-lang>`__ to construct any string value here.

.. rst-class:: api_tabs
.. tabs::
  .. tab:: Console

    In the ``Configure REST Connectors`` section, click on ``Add Request Options Transform``:

    .. thumbnail:: /img/graphql/core/actions/request-options-transform.png
       :alt: Change request URL
       :width: 800px

  .. tab:: CLI

    Will be added soon

  .. tab:: API

    .. code-block:: json
      :emphasize-lines: 3

      {
        "request_transform": {
          "url": "{{$base_url}}/{{$session_variables['x-hasura-user-id']}}",
          "query_params": {
             "param1": "{{$body.input.value1}}",
             "param2": "{{$body.input.value2}}"
          }
        }
      }

.. admonition:: escapeUri

  Note that you must use the ``escapeUri`` function to urlencode templated values.
  For example, if you have to use session variables in the URL and those may contain non-ASCII values,
  then you should provide the template URL as ``{{$base_url}}/{{escapeUri $session_variables['x-hasura-user-id']}}``

Method
******

Transform the method. This can be used to change the request method, say from ``POST`` to ``GET``, as shown below.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the ``Configure REST Connectors`` section, click on ``Add Request Options Transform``:

    .. thumbnail:: /img/graphql/core/actions/request-method-transform.png
       :alt: Change request method
       :width: 800px

  .. tab:: CLI

    Will be added soon

  .. tab:: API

    .. code-block:: json
      :emphasize-lines: 3

      {
        "request_transform": {
           "method": "GET"
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

    Will be added soon

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
