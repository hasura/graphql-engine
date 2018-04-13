Part IV: Build a blog app engine
================================

Let's build a simple clone of the popular publishing platform `medium <https://medium.com/>`_ and use that to learn about the features provided by the Hasura platform. Our clone blog app will have the following features:

#. Anyone can register on the site
#. Users can publish posts, like and comment
#. Anyone can view published posts, overall likes and comments


Hasura features
---------------

While making this simple application, these are the actions we will go through involving the features provided by Hasura:

#. Exploring the user & authentication Hasura APIs
#. Creating tables (on postgres) using the ``api-console`` UI
#. Exploring the hasura data APIs as an ``admin``
#. Adding permissions and access control on the data models
#. Adding relationships between the data models
#. Adding aggregations & views to compute derived information (eg: total likes)

Next: User & authentication APIs
--------------------------------

Next, let's head to :doc:`user-model`.
