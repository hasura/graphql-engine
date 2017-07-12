.. meta::
   :description: Reference documentation for hasura-project-secrets, a set of k-v pairs required for project config, its Kubernetes structure and JSON object schema.
   :keywords: hasura, docs, project configuration, proj conf, configuration, hasura-project-secrets

Project Secrets: ``hasura-project-secrets``
===========================================

These are a set of key value pairs that are required for project configuration 
that cannot/should not be used as raw values in a project configuration.

These are the default values in the hasura-project-secrets secrets object. 
You can add/remove values and refer to them from the hasura-project-conf.

These are represented as a kubernetes ``secret`` and should **NEVER** be checked into version control.

.. note:: To fetch the current desired project secrets on the cluster use the
  following kubectl command:

  ``kubectl get secrets hasura-project-secrets``


Kubernetes structure
--------------------

``hasura-project-secrets`` is a kubernetes secret.
Here’s a sample yaml file that will create a valid ``hasura-project-secrets`` resource.

.. code-block:: yaml

   apiVersion: v1
   data:
     auth.admin.password: YWRtaW5wYXNzd29yZA==
     auth.facebook.client_secret: IA==
     auth.github.client_secret: IA==
     auth.google.client_secret: IA==
     auth.linkedin.client_secret: IA==
     auth.msg91.key: YXNkZg==
     auth.recaptcha.secret: IA==
     auth.secretKey: MXEydzNlNHI1dDZ5N3U4aTlvMHA=
     auth.sparkpost.key: IA==
     postgres.password: cGdwYXNzd29yZA==
     postgres.user: YWRtaW4=
     ssh.authorizedKeys: IA==
   kind: Secret
   metadata:
     name: hasura-project-secrets
     namespace: default
   type: Opaque


Usage from ``hasura-project-conf``
----------------------------------
:doc:`hasura-project-conf <./hasura-project-conf>` is a kubernetes configmap that contains a JSON configuration object.

Here’s a portion of the hasura-project-conf JSON object using a password value directly in the object:

.. code-block:: JSON
   :emphasize-lines: 4

   {
      "services": {
         "postgres": {
           "password": "adminpassword"
         }  
      }
   }

Here’s the same JSON object, now referring to the ``postgres.password`` entry in the ``hasura-project-secrets`` kubernetes secret:

.. code-block:: json
   :emphasize-lines: 4-8

   {
      "services": {
         "postgres": {
           "password": {
               "secretKeyRef": {
                  "name": "hasura-project-secrets",
                  "key": "postgres.password",
               }
            }
         }
      }
   }

``secret`` Schema
-----------------

+--------------------+--------------------+--------------------+-------------------------------+
| Key                | Always present     | Schema             | Description                   |
+====================+====================+====================+===============================+
| auth.admin.passwor | true               | string             | Default:                      |
| d                  |                    |                    | ``YWRtaW5wYXNzd29yZA==``      |
|                    |                    |                    | (``base64("adminpassword")``) |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.facebook.clie | true               | string             | Default: ``IA==``             |
| nt\_secret         |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.github.client | true               | string             | Default: ``IA==``             |
| \_secret           |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.google.client | true               | string             | Default: ``IA==``             |
| \_secret           |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.linkedin.clie | true               | string             | Default: ``IA==``             |
| nt\_secret         |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.msg91.key:    | true               | string             | Default: ``IA==``             |
|                    |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.recaptcha.sec | true               | string             | Default: ``IA==``             |
| ret                |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.secretKey     | true               | string             | A base64 encoded              |
|                    |                    |                    | string, which                 |
|                    |                    |                    | should have been              |
|                    |                    |                    | generated using a             |
|                    |                    |                    | secure random                 |
|                    |                    |                    | generator                     |
+--------------------+--------------------+--------------------+-------------------------------+
| auth.sparkpost.key | true               | string             | Default: ``IA==``             |
|                    |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+
| postgres.password  | true               | string             | Default:                      |
|                    |                    |                    | ``cGdwYXNzd29yZA==``          |
|                    |                    |                    | (``base64("pgpassword")``)    |
+--------------------+--------------------+--------------------+-------------------------------+
| postgres.user      | true               | string             | Default:                      |
|                    |                    |                    | ``YWRtaW4=``                  |
|                    |                    |                    | (``base64("admin")``)         |
+--------------------+--------------------+--------------------+-------------------------------+
| ssh.authorizedKeys | true               | string             | Base64 encoded SSH            |
|                    |                    |                    | public keys                   |
|                    |                    |                    | seperated by                  |
|                    |                    |                    | ``\n``; Eg: a                 |
|                    |                    |                    | base64 encoding of            |
|                    |                    |                    | a typical                     |
|                    |                    |                    | ``.ssh/authorizedJKeys``      |
|                    |                    |                    | is a valid value.             |
|                    |                    |                    | Default empty                 |
|                    |                    |                    | value: ``IA==``               |
|                    |                    |                    | (``base64(" ")``)             |
+--------------------+--------------------+--------------------+-------------------------------+

