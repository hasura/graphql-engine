.. meta::
   :description: Hasura Cloud glossary
   :keywords: hasura, docs, cloud, glossary

.. _glossary:

Glossary
========

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Hasura Cloud Project
--------------------

A Project is an individual GraphQL API hosted by Hasura Cloud. You
can create a Project by going to the cloud dashboard and providing
a PostgreSQL database URL. You can also provision a database on
cloud platforms like Heroku from the Hasura Cloud Dashboard itself.

Each project is allocated a unique auto-generated name and an ID.
You can use this name or ID while communicating to Hasura team
regarding this project. Each project is also assigned a GraphQL API
endpoint of the format ``https://<project-name>.hasura.app/v1/graphql``.

Editing the Project name and adding custom domains will be available soon.

For example, a project might be called ``usable-cobra-29`` with ID
``bf0ea856-76a2-42c2-8a91-66ca9b9206e8``.

Hasura Cloud IP
---------------

A Hasura Cloud IP will be listed on the Hasura Cloud Dashboard for
each project. Hasura will be connecting to your database from this IP address.
If your database is not exposed to the internet, you must allow connections 
from this IP address on your firewall settings
for Hasura Cloud Project to function properly. Otherwise, Hasura will not 
be able to connect to your database and the GraphQL API will not be available.

Hasura Collaborator Token
-------------------------

When you open the Hasura Console on a Cloud Project, you will not be asked to
enter the admin secret like Hasura Core version. Instead, you will be
automatically logged into the Console via an OAuth2.0 based authorization flow.
You will be given the right access based on your permissions for the particular
Hasura Cloud Project.

After the login process is complete, you'll see a new header called
``Hasura-Collaborator-Token`` in the "Request Headers" section of GraphiQL. 
This token is used instead of admin secret to authenticate and authorize
all the requests made from the Console. The token is only valid for 5mins
and is refreshed silently by the Console. It is to be used only from Console.

For accessing the API from other clients, use the admin secret or create
a Personal Access Token.

Hasura Client Name
------------------

``Hasura-Client-Name`` will be set to ``hasura-console`` by default. It is
used to identify the client who is making the request in Hasura Pro metrics
and monitoring tools.
