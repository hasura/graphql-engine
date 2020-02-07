Input Types
============

You can pass complex objects as arguments to a mutation. This is particularly valuable in the case of mutations, where you might want to pass in a whole object to be created. In the GraphQL SDL, input types look exactly the same as regular object types, but with the keyword input instead of type:

.. code-block:: graphql

    input LoginInfo {
      username: String!
      password: String!
    }

A field of an input type could be a ``scalar``, an ``enum`` or another input type.

