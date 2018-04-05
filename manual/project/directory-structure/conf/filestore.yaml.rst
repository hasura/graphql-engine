.. _hasura-dir-conf-filestore.yaml:

Project structure: conf/filestore.yaml
======================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

.. code-block:: yaml

   hookUrl: http://localhost:8080/v1/hooks/public-read-user-write
   volume: {{ cluster.metadata.filestore.volume|json }}

hookUrl
-------

This field defines the URL to be contacted for enforcing permissions on uploading and downloading files. If no hookUrl is mentioned, it defaults to admin-only.

The filestore service has the following built in hooks:

- Private: Only logged in users can read and upload (``http://localhost:8080/v1/hooks/user-read-write``)

- Public: Anybody can read, but only logged in users can upload (``http://localhost:8080/v1/hooks/public-read-user-write``)

- Read Only: Anybody can read, but no one can upload (``http://localhost:8080/v1/hooks/public-read``)

- Custom Permission URL: For any other custom permissions, you need to define your own microservice. More details can be found at :doc:`Authorization webhooks <../../../filestore/webhook>`.

volume
------

Volume defines the Kubernetes volume to be mounted for storing the files, this is usually filled in from the cluster metadata.

You can find the default file at `conf/filestore.yaml <https://github.com/hasura/base/blob/master/conf/filestore.yaml>`_ in the base repo.

