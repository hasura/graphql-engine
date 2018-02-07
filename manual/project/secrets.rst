.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasura-secrets-manual:

.. highlight:: bash

Project secrets
===============

Project secrets are variables that your microservices need, but variables that are not safe to put in your git repository.
Here are some typical examples:

- Postgres (database) password
- API tokens for 3rd party APIs

Hasura gives you a simple way of managing your secrets directly on your cluster (via Kubernetes secrets).

List secrets
------------
To list the secrets currently available on the cluster:

.. code-block:: bash

   $ hasura secrets

   INFO Fetching secrets...
   auth.secretKey----------------|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   notify.sparkpost.key----------| 
   notify.twilio.accountsid------| 
   auth.linkedin.client_secret---| 
   auth.github.client_secret-----| 
   auth.sparkpost.key------------| 
   postgres.password-------------|xxxxxxxxxxx-xxxxxxxxxxx-xxx-xxxxxxxxxx
   postgres.user-----------------|admin
   auth.msg91.key----------------| 
   notify.twilio.authtoken-------| 
   notify.msg91.key--------------| 
   auth.facebook.client_secret---| 
   auth.google.client_secret-----| 
   ssh.authorizedKeys------------| 
   notify.smtp.password----------| 
   notify.smtp.username----------| 
   auth.recaptcha.secret---------| 
   auth.admin.password-----------|xxxxxxxxxxxx-xxxxxxxxxxxxx-xxxxxxxxx-xxxx

Using the secrets in your configuration
---------------------------------------

Let's say you have a microservice called ``api`` that needs the postgres password to access the database directly.
You can edit the ``microservices/api/k8s.yaml`` to add an environment variable that refers to the this secret.
When the microservice container is run, the value of the secret will be made available as an environment variable to
the container.

.. code-block:: yaml
   :emphasize-lines: 17-22

   apiVersion: v1
   items:
   - apiVersion: extensions/v1beta1
     kind: Deployment
     metadata:
       ...
     spec:
       ...
       template:
         metadata:
           ...
         spec:
           containers:
           - image: hasura/hello-world:latest
             imagePullPolicy: IfNotPresent
             name: ui
             env:
             - name: DB_PASSWORD
               valueFrom:
                 secretKeyRef:
                   name: hasura-secrets
                   key: postgres.password
             ports:
             - containerPort: 8080
               protocol: TCP
             resources: {}
           securityContext: {}
           terminationGracePeriodSeconds: 0
   ...

Add/update secrets
------------------
You can add new or modify old secrets by using this command:

.. code-block:: bash

   $ hasura secrets update my.new.key 1234-some-secret-value-6789

Here ``my.new.key`` is the name of the secret and ``1234-some-secret-value-6789`` is the value of the secret.

List secrets
------------
To see all the secrets that are saved in your cluster:

.. code-block:: bash

   $ hasura secrets list
