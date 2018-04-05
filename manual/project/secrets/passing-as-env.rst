Passing project secrets as environment variables
================================================

Let's say you have a microservice called ``<my-microservice>`` that needs the secret ``<my.secret.var>`` in its code.
You can edit the ``microservices/<my-microservice>/k8s.yaml`` to add a variable ``<MY_SECRET_VARIABLE>`` that refers to this secret.
When the microservice container is run, the value of the secret will be made available as an environment variable to
the container called ``MY_SECRET_VARIABLE``.

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
             - name: <MY_SECRET_VARIABLE>
               valueFrom:
                 secretKeyRef:
                   name: hasura-secrets
                   key: <my.secret.var>
             ports:
             - containerPort: 8080
               protocol: TCP
             resources: {}
           securityContext: {}
           terminationGracePeriodSeconds: 0
   ...


See :doc:`this <../../microservices/env-variables>` for more info on passing ENV variables to microservices.
