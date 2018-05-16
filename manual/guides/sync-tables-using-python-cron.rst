Using a python cron to sync data between tables
===============================================

This section will talk about how you can use a python cron to sync data between tables.

For the purpose of this example, we are going to consider two tables ``tableA`` and ``tableB``. Both the tables will have the same schema

+----------------------------------------+----------------------------------------+
|Column                                  |Type                                    |
+========================================+========================================+
|id                                      |Integer (Primary Key)                   |
+----------------------------------------+----------------------------------------+
|value                                   |Text                                    |
+----------------------------------------+----------------------------------------+
|last_updated                            |Timestamp (Default: now())              |
+----------------------------------------+----------------------------------------+

Here, we want to ensure that the data between ``tableA`` is always synced with ``tableB``.

It is important to have some form of indication to know what has changed in ``tableA``, so that we can insert/update this into ``tableB``. In this case, we are going with the ``last_updated`` column.
We will ensure that for every change on ``tableA`` will update the ``last_updated`` value for that row. This way, we only need to fetch data whose ``last_updated`` timestamp value is greater than the latest ``last_updated`` timestamp on ``tableB``.

Similarly, you can have other patterns like maintaining an ``is_synced`` or ``status`` field.

Handling deletes also can be made simple by having an ``is_deleted`` column in ``tableA``. So instead of actually deleting the row, you would update its ``is_deleted`` value to true and then handle this change on ``tableB``.

To implement our simple example of syncing data between ``tableA`` and ``tableB``, we are going to do the following:
  - Clone an existing python cron microservice from Hasura Hub into our Hasura Project.
  - Create a new file where the code to sync the tables goes in.
  - Syncing Logic would be as follows

    * Fetch the row from ``tableB`` with the latest value for ``last_updated``
    * Fetch all rows from ``tableA`` after the latest ``last_updated`` timestamp of ``tableB``
    * Insert/Update this data into ``tableA``

Alright, let's get started.

Cloning a python cron microservice from Hasura Hub
--------------------------------------------------

Clone the ``cron`` microservice from `hasura/python-cron <https://hasura.io/hub/projects/hasura/python-cron>`_ into your project by following the
instructions :doc:`here <../microservices/add-microservice/using-template>`

.. note::

   Since this microservice is a cron job we do not need to expose it to an external URL. You can skip step 4 in the instructions to clone a microservice.

Next, navigate to ``microservices/cron/src`` and add a new file called ``table_sync.py``.

Let's now add our logic to this file.

Fetching latest value of ``last_updated`` from ``tableB``
---------------------------------------------------------

.. code::

  def getLastUpdatedTimestamp() :
    tableBRequestPayload = {
        "type": "select",
        "args": {
            "table": "tableB",
            "columns": [
                "last_updated"
            ],
            "order_by": ["-last_updated"],
            "limit": "1"
        }
    }
    lastUpdatedTableBData = requests.request("POST", url, data=json.dumps(tableBRequestPayload), headers=headers)
    if (lastUpdatedTableBData.status_code != 200) :
        print("Failed to fetch data from tableB")
        return

    jsonData = lastUpdatedTableBData.json()
    lastUpdatedTimestamp = jsonData[0]['last_updated'] if len(jsonData) > 0 else None
    return lastUpdatedTimestamp

The ``order_by`` key is used to order the results in the descending order of ``last_updated``. The ``-`` symbol is used for descending and a ``+`` symbol for ascending. The ``limit`` key specifies the number of rows we want, in this case ``1``, thereby fetching us the row with the latest value of ``last_updated``.

Fetching new values from ``tableA`` after latest timestamp
----------------------------------------------------------

.. code::

  def getAllDataFromTimestamp(lastUpdatedTimestamp):
    requestPayload = {
        "type": "select",
        "args": {
            "table": "tableA",
            "columns": [
                "*"
            ]
        }
    }
    if (lastUpdatedTimestamp is not None):
        requestPayload['args']['where'] = {
            "last_updated": {
                "$gt": lastUpdatedTimestamp
            }
        }
    response = requests.request("POST", url, data=json.dumps(requestPayload), headers=headers)
    if (response.status_code != 200) :
        print("Failed to fetch data from tableA")
        print(response.content)
        return

    jsonData = response.json()
    return jsonData

If ``lastUpdatedTimestamp`` does not have a value then fetch everything from ``tableA``. The ``where`` condition specifies that we want to fetch rows whose ``last_updated`` timestamp is greater than the value provided.

Upserting data into ``tableB``
------------------------------

.. code::

  def upsertDataToTableB(newData):
      # Perform operations on new data if required or sync as is
      requestPayload = {
          "type": "insert",
          "args": {
              "table": "tableB",
              "objects": newData,
              "on_conflict": {
                  "action": "update",
                  "constraint_on": [
                      "id"
                  ]
              }
          }
      }
      response = requests.request("POST", url, data=json.dumps(requestPayload), headers=headers)
      if (response.status_code != 200) :
          print("Failed to insert data into tableB")
          print(response.content)
          return

      jsonData = response.json()
      return jsonData


The value of ``on_conflict`` specifies that whenever the `unique key` constraint on the ``id`` column is violated, update the row with the data in the request. A new row is inserted if there is no conflict.
The ``newData`` being passed to this function is the response from the previous function ``getAllDataFromTimestamp``.

.. note::

  You can also modify the value of ``newData`` according to your needs and then insert it into your "clone table" (``tableB`` in this case). Ensure that the schema of ``tableB`` is modified as needed.

The complete code for this file can be found `here <https://gist.github.com/jaisontj/725f8bf6a038d805958efa1168672972>`_.

Deploying the microservice
--------------------------

Before we deploy this microservice, we also need to make a small change in the ``main.py`` file to run our newly create ``table_sync.py`` file every 1 minute.

Open ``microservices/cron/src/main.py`` and replace ``hello.py`` with ``table_sync.py`` in line 7.

Your ``main.py`` will look like:

.. code::

  from crontab import CronTab

  # File name for cron
  my_cron = CronTab(tabfile='my_cron.tab')

  # Add cron command and time span
  job  = my_cron.new(command='python /usr/src/app/table_sync.py')
  job.minute.every(1)

  # Write cron jobs to cron tab file
  my_cron.write()

  # Run the scheduler
  for result in my_cron.run_scheduler():
    print (result)


To deploy the microservice, simply ``git push``.

.. code:: bash

  git add . && git commit -m "python sync cron"
  git push hasura master
