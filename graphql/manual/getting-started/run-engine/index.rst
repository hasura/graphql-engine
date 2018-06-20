Run Hasura GraphQL Engine
=========================

The fastest way to get the engine up and running is by clicking the below button:

.. image:: https://camo.githubusercontent.com/83b0e95b38892b49184e07ad572c94c8038323fb/68747470733a2f2f7777772e6865726f6b7563646e2e636f6d2f6465706c6f792f627574746f6e2e737667
  :alt: heroku_deploy_button
  :target: https://heroku.com/deploy?template=https://github.com/karthikvt26/heroku-push

.. note::
   This will setup an instance of Postgres, an instance of Hasura GraphQL engine, and link the two on `Heroku
   <https://www.heroku.com/platform>`_ 's free tier.

You can also run the Hasura GraphQL engine anywhere else using Docker.

The following other options are available to run the Hasura GraphQL engine:

- :doc:`Manually on Heroku <on-heroku>`
- :doc:`Using docker <using-docker>`
- :doc:`Using docker-compose <using-docker-compose>`

.. toctree::
  :maxdepth: 1
  :hidden:

  On Heroku <on-heroku>
  Using docker compose <using-docker-compose>
  Using docker <using-docker>
