=====================================
Customise Schema using Postgres Views
=====================================

In the previous steps, we have created tables, added foreign keys, relationships and applied permissions as well. GraphQL APIs were automatically generated based on the available schema. There are instances where you would like to fetch data from multiple tables based on various conditions and joins or if you have a lot of complex calculations.

**What are views in SQL?**

Views are a virtual table based on the result of a SQL statement. It is just like a real table except for the fact that it consists of data from one or more tables.

**When are views useful?**

To simplify complex SQL statements.
If you have a query that fetches data from multiple tables based on various conditions and joins or if you have a lot of complex calculations or logic with your query, you can create a view and then directly SELECT that data from a view.

**Security**

Let’s say you have multiple tables which has both public and private data. In this case, you can create a view with just the public data from these tables and only expose this view.

Now to cater to above use cases, we can create views in Postgres and expose them via GraphQL APIs for querying.

**Note:** Views are read-only data. Mutations cannot be performed since views are calculated data.

Creating a view
---------------

We already have article, author, like and comment tables., Let’s create a view which shows the total likes for each article.

The SQL statement to create this view will be

.. code-block:: sql

    CREATE VIEW article_like_count AS 
    SELECT article_id, COUNT(user_id) as total_likes
    FROM like
    GROUP BY article_id;

To run this SQL statement, head to the Data tab in ``api-console`` and click on SQL from the panel on the left.

Note: Ensure that you check the ``This is a migration`` and ``Track Table`` checkbox.

Fetching data from view
-----------------------

To fetch the total likes for a particular article 

.. code-block:: none

    query fetch_likes {
      article_like_count(where: { article_id: 1 }) {
        article_id
        total_likes
      }
    }

Fetching total likes for an article with id 1 

Alternatively, you can also add this view as a relationship to the article table and fetch the article details as well the total likes in one query.

Head to the Data tab and click on article. Click on the Relationship tab and hit the Add a manual relationship button.

In the form that comes up, select the following:

Now, you can query the article table for articles along with their total_likes

.. code-block:: none

    query fetch_articles {
      article (order_by: ["-likes.total_likes"]){
        id
        author_id
        title
        content
        created_at
        likes {
          total_likes
        }
      }
    }

**Note:** The ``order_by`` condition is used to list the articles ordered by the number of likes it has received. - sign is to list them in descending order and + will be for ascending.

