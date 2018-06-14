Golang
======

Using gin
---------

`codegangsta/gin <https://github.com/codegangsta/gin>`_ is a live reload utility
for Go web servers. It can run any ``main.go`` file and restart it when
dependent code changes. gin runs a webserver on it's own and proxies requests to
the actual webserver. The only requirement is that the actual webserver should
take a ``PORT`` environment variable and listen on that address so that gin can
set it arbitrarily.

Let's look at a typical Go microservice (refer `hasura/hello-golang-raw
<https://hasura.io/hub/project/hasura/hello-python-flask>`_).

.. code-block:: bash

   .
  ├── Dockerfile
  ├── k8s.yaml
  └── src
      └── main.go

If you run ``gin --path src --port 8080 run main.go``, a webserver will be
started in port 8080 and it will restart automatically for any code change.

.. code-block:: dockerfile
   :caption: Dockerfile

   # Step 1: use golang base image
   FROM golang:1.8.5-jessie

   # Step 2: install gin
   RUN go get github.com/codegangsta/gin
   
   # Step 3: setup the working directory
   WORKDIR /go/src/app

   # Step 4: add source code
   ADD src src
   
   # Step 5: start a gin server
   CMD ["gin", "--path", "src", "--port", "8080", "run", "main.go"]


Step 4 copies the source code from ``src`` to ``/gp/src/app/src``. Sync for
these directories can be done using the following command (assuming name of the
microservice is ``api``):

.. code-block:: bash

   $ hasura microservice sync api microservices/api/src:/go/src/app/src

Keep the command running in a terminal. Open the microservice in a browser,
using another terminal window:

.. code-block:: bash

   $ hasura microservice open api

Make some changes to ``main.go`` and save. Refresh the browser and voila! You
can see that changes live in the browser tab.

