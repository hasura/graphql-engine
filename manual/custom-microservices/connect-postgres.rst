.. .. meta::
   :description: Connecting to Hasura PostgreSQL database from your microservice deployed on Hasura
   :keywords: hasura, microservice, postgres

.. _connecting-to-postgres:

Connecting to Hasura PostgreSQL database from microservice
==========================================================

To connect to any PostgreSQL instance, we need the hostname, port, username and password. You can find out hostnames (URLs) for all the microservices running in the cluster by executing the following command:

.. code-block:: bash
   :emphasize-lines: 16

   $ hasura microservice list
   • Getting microservices...
   • Custom microservices:
   NAME   STATUS    INTERNAL-URL   EXTERNAL-URL
   app    Running   app.default    http://app.gram29.hasura-app.io

   • Hasura microservices:
   NAME            STATUS    INTERNAL-URL           EXTERNAL-URL
   auth            Running   auth.hasura            http://auth.gram29.hasura-app.io
   data            Running   data.hasura            http://data.gram29.hasura-app.io
   filestore       Running   filestore.hasura       http://filestore.gram29.hasura-app.io
   gateway         Running   gateway.hasura         
   le-agent        Running   le-agent.hasura        
   notify          Running   notify.hasura          http://notify.gram29.hasura-app.io
   platform-sync   Running   platform-sync.hasura   
   postgres        Running   postgres.hasura        
   session-redis   Running   session-redis.hasura   
   sshd            Running   sshd.hasura        

As we can see from the list, Postgres microservice is available at "Internal URL" (hostname) ``postgres.hasura``. Internal URL should be used while accessing any microservice from inside the cluster. Since our microservice is going to be deployed on the cluster, we should use the internal URL. The port is ``5432``.
               
Username and password can be accessed through Hasura Secrets.

.. code-block:: bash
   :emphasize-lines: 7-8

   $ hasura secrets list
   auth.facebook.client_secret---| 
   notify.smtp.password----------| 
   auth.admin.password-----------|xxxxxxxxxxxxxxxxxxxxxxxx
   notify.sparkpost.key----------| 
   auth.secretKey----------------|xxxxxxxxxxxxxxxxxxxxxxxx
   postgres.password-------------|xxxxxxxxxxxxxxxxxxxxxxxx
   postgres.user-----------------|xxxxx
   auth.linkedin.client_secret---| 
   notify.twilio.authtoken-------| 
   auth.msg91.key----------------| 
   auth.sparkpost.key------------| 
   notify.smtp.username----------| 
   auth.google.client_secret-----| 
   ssh.authorizedKeys------------| 
   auth.github.client_secret-----| 
   auth.recaptcha.secret---------| 
   notify.twilio.accountsid------| 
   notify.msg91.key--------------| 

While you can add hostname, port, username, password to your code, it is not recommended to do so. Especially username, password etc. should not be added as it is to your code, since they are going to be committed into a git repository.

The standard practice is to access these parameters as environment variables and Hasura provides a convenient method to do so. Let's look at the steps to add these environment variables.

(The following section assumes that you have a Hasura cluster and project ready, with at least one microservice [say ``app``])

1. Edit ``k8s.yaml`` file inside ``microservices/app`` directory and add the highlighted code:

   .. code-block:: yaml
      :emphasize-lines: 23-37
   
      apiVersion: v1
      items:
      - apiVersion: extensions/v1beta1
      kind: Deployment
      metadata:
        creationTimestamp: null
        labels:
          app: app
          hasuraService: custom
        name: app
        namespace: '{{ cluster.metadata.namespaces.user }}'
      spec:
        replicas: 1
        strategy: {}
        template:
          metadata:
            creationTimestamp: null
            labels:
              app: app
          spec:
            containers:
            - image: hasura/hello-world:latest
              env:
              - name: POSTGRES_HOSTNAME
                value: postgres.{{ cluster.metadata.namespaces.hasura }}
              - name: POSTGRES_PORT
                value: "5432"
              - name: POSTGRES_USERNAME
                valueFrom:
                  secretKeyRef:
                    name: hasura-secrets
                    key: postgres.user
              - name: POSTGRES_PASSWORD
                valueFrom:
                  secretKeyRef:
                    name: hasura-secrets
                    key: postgres.password
              imagePullPolicy: IfNotPresent
              name: app
              ports:
              - containerPort: 8080
                protocol: TCP
              resources: {}
            securityContext: {}
            terminationGracePeriodSeconds: 0
      status: {}
      - apiVersion: v1
      kind: Service
      metadata:
        creationTimestamp: null
        labels:
          app: app
          hasuraService: custom
        name: app
        namespace: '{{ cluster.metadata.namespaces.user }}'
      spec:
        ports:
        - port: 80
          protocol: TCP
          targetPort: 8080
        selector:
          app: app
        type: ClusterIP
      status:
        loadBalancer: {}
      kind: List
      metadata: {}

2. Commit and push your code:

   .. code-block:: bash

      $ git add microservices/app/k8s.yaml
      $ git commit -m "add postgres credentials"
      $ git push hasura master

3. Hasura will make the following environment variables available for the microservice to use:

   * ``POSTGRES_HOSTNAME``
   * ``POSTGRES_PORT``
   * ``POSTGRES_USERNAME``
   * ``POSTGRES_PASSWORD``

   Depending on your application code, you can make use of these environment variables to connect to Hasura PostgreSQL database.

   .. note::

      Name of the database available through Hasura API Console and Hasura Data API is ``hasuradb``


   An example with Python and psycopg2:

   .. code-block:: python

      import os
      import psycopg2


      conn = psycopg2.connect(
          database='hasuradb',
          user=os.environ['POSTGRES_USERNAME'],
          password=os.environ['POSTGRES_PASSWORD'],
          host=os.environ['POSTGRES_HOSTNAME'],
          port=os.environ['POSTGRES_PORT']
      ) 
