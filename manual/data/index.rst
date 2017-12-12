.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Data
====

This section assumes that you have already created a project and know how to
launch your API console. If not, please head to :ref:`getting-started`.

The data APIs can be used to store and retrieve data. The actual data is stored
in Postgres tables. To fetch associated data, one can define relationships on
tables. Permissions can then be used to authorize the access to data based on
roles.

.. toctree::
  :maxdepth: 1

  Create tables <create-tables>
  Select & count query <select>
  Update query <update>
  Insert query <insert>
  Delete query <delete>
  Bulk query<bulk>
  Permissions and access control<permissions>
  Relationships <relationship>
  Altering schema <alter-schema/index>
  Linking auth users to data <linking_users_auth>
  Using SQL directly on data <using-sql>
  Data/schema migrations <data-migration>
  Reset Database <db-reset>
  Aggregations <aggregations>
  Accessing postgres directly <access-postgres>
  Importing data from SQL files <import-sql>
