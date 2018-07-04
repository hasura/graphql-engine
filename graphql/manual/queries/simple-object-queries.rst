Simple object queries
=====================

You can fetch a single node or multiple nodes of the same type using a simple object query. 

E.g. Fetch a list of authors:

.. graphiql::
  :query:
    query {
      author {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Chrissie"
          },
          {
            "id": 2,
            "name": "Aubrey"
          },
          {
            "id": 3,
            "name": "Mallorie"
          },
          {
            "id": 4,
            "name": "Axel"
          },
          {
            "id": 5,
            "name": "Dreddy"
          },
          {
            "id": 6,
            "name": "Bernhard"
          },
          {
            "id": 7,
            "name": "Eleonore"
          },
          {
            "id": 8,
            "name": "Khalil"
          },
          {
            "id": 9,
            "name": "Dorris"
          },
          {
            "id": 10,
            "name": "Obie"
          },
          {
            "id": 11,
            "name": "Rubi"
          },
          {
            "id": 12,
            "name": "Ricoriki"
          },
          {
            "id": 13,
            "name": "Quintus"
          },
          {
            "id": 14,
            "name": "Chrotoem"
          },
          {
            "id": 15,
            "name": "Ericka"
          },
          {
            "id": 16,
            "name": "Catherin"
          },
          {
            "id": 17,
            "name": "Lin"
          },
          {
            "id": 18,
            "name": "Marten"
          },
          {
            "id": 19,
            "name": "Lida"
          },
          {
            "id": 20,
            "name": "Saunderson"
          },
          {
            "id": 21,
            "name": "Sophey"
          },
          {
            "id": 22,
            "name": "Conny"
          },
          {
            "id": 23,
            "name": "Edithe"
          },
          {
            "id": 24,
            "name": "Jeri"
          },
          {
            "id": 25,
            "name": "Niki"
          },
          {
            "id": 26,
            "name": "Wenda"
          },
          {
            "id": 27,
            "name": "Ashby"
          },
          {
            "id": 28,
            "name": "Derril"
          },
          {
            "id": 29,
            "name": "Carmella"
          }
        ]
      }
    }