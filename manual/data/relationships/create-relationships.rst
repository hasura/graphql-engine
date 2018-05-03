.. _data-create-relationships:

Creating data relationships
===========================

You can create relationships for tables via the :doc:`API console <../../api-console/index>`. The Data microservice
automatically detects and suggests possible relationships based on foreign keys. But it is not always possible to connect
tables via foreign keys. Hence you can also create relationships without using foreign keys. This is typically required
when you want a relationship between a table and a view as we cannot create foreign keys to/on views.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Using Foreign Keys

      Let's say you wish to add an object relationship based on the foreign key ``article::author_id -> author::id``.
      Navigate to the *Relationships* tab of the ``article`` table.

      You'll see an entry in *suggested object relationships*:

      .. image:: img/suggested-relationships.png

      Click on *Add* to add a new object relationship and give it the name ``author``:

      .. image:: img/create-relationship.png

      The relationship is created:

      .. image:: img/created-relationship.png

   .. tab:: Without Foreign Keys

      Let's say you have an ``article`` table and a ``article_total_likes`` view which has the total number of likes each article has received. To create an object relationship for ``article::id -> article_total_likes::article_id``:

      Navigate to the ``relationship`` tab after selecting ``article`` and click on ``+ Add a manual relationship``

      .. image:: img/manual-relationship.png

      - Relationship Type will be ``Object Relationship``
      - Relationship Name can be "total_likes"
      - Configuration: ``id :: article_total_likes -> article_id``

Once this is done, you can fetch data over it like any relationship, as shown
:doc:`here <fetch-over-relationships>`.
