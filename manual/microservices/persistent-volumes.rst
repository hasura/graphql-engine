Adding persistent volumes to microservice
=========================================

You might want to use a persistent volume for your microservice to persist data.
For example, you might want to run mysql or ghost blog and persist the data.

By default, microservices that you create are not persistent. Any file that you
save in the filesystem inside your code is not persisted. If the microservice
restarts, the files/data would get destroyed with the container.

To persist any data, you should use persistent volumes. This can be done in
pro-tier clusters currently.

To use persistent volumes:

1. Create a new volume in a pro-tier cluster.
2. Configure your microservice in ``k8s.yaml`` to use the volume.


Create a new volume
--------------------
Edit your ``clusters.yaml`` and add a new volume of your chosen size.

Example:

.. code-block:: yaml
   :emphasize-lines: 15-17

   version: v1
   provider: digital-ocean
   region: sfo2
   nodes:
   - type: s-2vcpu-4gb
     labels:
       app: postgres
   volumes:
   - name: postgres
     size: 10
   - name: filestore
     size: 10
   - name: sessionstore
     size: 2
   # custom volume
   - name: myvolume
     size: 10


Apply this config to your cluster by running the command:

.. code-block:: shell

   $ hasura cluster upgrade -c <cluster-alias>


Use the volume in your microservice
-----------------------------------

You can configure persistent volumes for a microservice by editing the
``k8s.yaml`` file found at ``/microservices/<my-microservice>/k8s.yaml``.

Let's say you need use ``myvolume``, created in the previous step, in a ghost
blog microservice.

The ``k8s.yaml`` for such a configuration would look like the following:

.. code-block:: yaml+jinja
   :emphasize-lines: 24-28

      apiVersion: extensions/v1beta1
      kind: Deployment
      metadata:
        labels:
          app: '{{ microservice.name }}'
          hasuraService: custom
        name: '{{ microservice.name }}'
        namespace: '{{ cluster.metadata.namespaces.user }}'
      spec:
        replicas: 1
        template:
          metadata:
            creationTimestamp: null
            labels:
              app: '{{ microservice.name }}'
          spec:
            containers:
            - image: ghost:1-alpine
              imagePullPolicy: IfNotPresent
              name: '{{ microservice.name }}'
              ports:
              - containerPort: 2368
                protocol: TCP
              volumeMounts:
                - name: myvolume
                  mountPath: /var/lib/ghost/content
            volumes:
            - {{ cluster.metadata.volumes.myvolume | json }}


These are the keys that has to be filled:

* ``volumeMounts``:

  * ``mountPath`` : is the path **inside your container** where you want the
    data to be persisted
  * ``name`` : is the name of the volume you created in ``clusters.yaml``.

* ``volumes`` : a template variable of the form
  ``cluster.metadata.volumes.<volume-name>``. In this case,
  ``cluster.metadata.volumes.myvolume``.


To apply the changes, commit the ``k8s.yaml`` file and ``git push hasura
master`` to deploy.
