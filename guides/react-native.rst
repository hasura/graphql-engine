:orphan:

.. meta::
   :description: A guide to integrate a React Native application with Hasura
   :keywords: hasura, guide, react, native, react-native, expo,
   :content-tags: react-native

.. title:: React Native app with Hasura

.. rst-class:: guide-title
.. rubric:: React Native app with Hasura

Getting started with React Native
---------------------------------

This section provides a start guide for start building a mobile application using React Native. We will use React Native, Expo and Hasura to run a fully functional To-Do application.

If you already have a working React Native application and want to integrate your application with a Hasura back-end, please move on to the next section.

Setting up the Expo Development Environment (XDE)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. Download the Expo XDE from `here <https://expo.io/tools>`_.

2. Open the XDE. Linux users, open using ``chmod a+x xde*.AppImage`` and ``./xde*.AppImage``.

3. Login. Register if you don't have an account.

4. Install *Expo* on your phone from AppStore/Playstore.

Getting the app running
^^^^^^^^^^^^^^^^^^^^^^^

1. Clone `this <https://github.com/hasura/expo-hasura.git>`_ git repository.

.. code:: bash

    $ git clone https://github.com/hasura/expo-hasura.git

2. Enter the app directory and install the dependencies.

.. code:: bash

    $ cd expo-hasura
    $ npm install

3. Go to XDE. Choose ``Open Existing Project`` and open the app directory.

4. Your app is up and running.

5. Click on the ``Share`` button on the XDE. Scan this QR code from your Expo app on the phone.

The app will open on your phone. However, in order to use login and be able to make data queries, you need to integrate your app with Hasura. Jump to "Integrating with Hasura" to quickly get it ready.

Making the app public
^^^^^^^^^^^^^^^^^^^^^

When you share the app using the QR code, you can open the app only if the phone is on the same connection as that of your computer. In order to make your app accessible from everywhere, click on the ``Publish`` button on the XDE. Now everybody in the world can open the app as long as they have the QR code and the Expo app.

To know more about Expo, go to the `expo docs <https://docs.expo.io/versions/latest/index.html>`_.

Building an expo app backend with Hasura
----------------------------------------

Hasura provides an easy way to build powerful backends for applications. They are easy to set up, configure and access. Follow these steps to itegrate a Hasura backend with your application.

Creating a Cluster
^^^^^^^^^^^^^^^^^^

1. Login to `dashboard.hasura.io <https://dashboard.hasura.io>`_ and activate your free trial project.

2. Login to your project console. You'll receive an email in your inbox with credentials to your new project.

   Let's say your project name is ``test42``.
   Login in to `console.test42.hasura-app.io <https://console.test42.hasura-app.io>`_ with the admin username
   and password from the email sent to you.

6. If you are coming from the *Getting started to react native* guide: modify the cluster name in ``hasuraAPI.js`` file.

.. code:: javascript

    const CLUSTER_NAME = '<cluster-name>';
    // replace <cluster-name> with your project name.
    // For eg: "test42"

Adding Authentication to your expo app
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Hasura Auth API is a handy way to include authentication in your application. Tasks such as login, signup, session management are straightforward. There is no need to manage users in database.

**Note**: If you are coming from the *Getting started with React Native* guide above, this part has already been implemented for you in the app.

1. The code snippet below shows how to make a signup request. This will create a new user and return an access token and a user_id. You can store these on the device to make further queries.

.. code:: javascript

    async function trySignup(inUsername, inPassword){
      let response = await fetchUrl("https://auth.<cluster-name>.hasura-app.io/signup", {
        method: 'POST',
        headers: {
          'content-type': 'application/json'
        },
        body: JSON.stringify({
          username: inUsername,
          password: inPassword,
        })
      });
      return response;
    }

2. To make a login request, use the following method. This will login the user and return an access token and the user_id of the user that logged in.

.. code:: javascript

    async function trySignup(inUsername, inPassword){
      let response = await fetchUrl("https://auth.<cluster-name>.hasura-app.io/login", {
        method: 'POST',
        headers: {
          'content-type': 'application/json'
        },
        body: JSON.stringify({
          username: inUsername,
          password: inPassword,
        })
      });
      return response;
    }

3. To obtain the access token and id from the login/signup response, use the following snippet.

.. code:: javascript

    var response = await tryLogin(username, password);
    var respJson = response.json();
    var accessToken = respJson.auth_token;
    var user_id = respJson.hasura_id;

4. Logout. While logging out, make sure you delete the access token and user_id from wherever you are storing it on the device.

.. code:: javascript

    async function tryLogout(accessToken){
      await fetchUrl("https://auth.<cluster-name>.hasura-app.io/user/logout", {
        method: 'GET',
        headers: {
          'Authorization': 'Bearer '+accessToken
        },
      });
    }

**Note**: To read more about authentication, roles and sessions, head on to `docs <https://docs.hasura.io/0.15/manual/users/index.html>`_.

Adding a table
^^^^^^^^^^^^^^

This section demonstrates adding a table to your database. We will do this by adding a simple table that is used in the "Getting started with React Native" guide.

1. Open the project console. Say your project is ``test42``, head to `console.test42.hasura-app.io <https://console.test42.hasura-app.io>`_.

2. Go to ``Data`` -> ``Add table`` and add the table as shown below.

.. image:: ../img/guide-react-native-add-table.png

3. Modify the permissions such that users can only access the elements associated with their user id.

.. image:: ../img/guide-react-native-permissions.png

4. You're set. Lets see how to make queries on this table next.

Data Queries
^^^^^^^^^^^^

1. Lets start by inserting an element in the ``todo`` table that we created in last section. Let us add a task with ``name = "To hack"``, ``completed = flase``, ``user_id = 4``.

.. code:: javascript

    export async function insertQuery(accessToken, my_user_id){
      let response = await fetchUrl('https://data.<cluster-name>.hasura-app.io/v1/query', {
        method: 'POST',
        headers: {
          'content-type': 'application/json',
          'Authorization': 'Bearer ' + authToken,
        },
        body: JSON.stringify({
          type: 'insert',
          args: {
            table: 'todo',
            objects: [{
              name: "To hack",
              completed: false,
              user_id: 4,
            }]
          }
        })
      })
    }

2. If you want to select all the tasks added by you, you can run a select query by the following function.

.. code:: javascript

    export async function insertTodoToDB(accessToken, my_user_id){
      let response = await fetchUrl('https://data.<cluster-name>.hasura-app.io/v1/query', {
        method: 'POST',
        headers: {
          'content-type': 'application/json',
          'Authorization': 'Bearer ' + authToken,
        },
        body: JSON.stringify({
          type: 'select',
          args: {
            table: 'todo',
            columns: ['*']
          },
          where: {
            user_id: userId
          }
        })
      })
    }

The response is a JSON Array with all the entries where ``user_id = my_user_id``. For example:

.. code:: json

    [
      {
        "name": "To Hack",
        "completed": true,
        "id": 17,
        "user_id": 2
      },
      {
        "name": "Or not to hack",
        "completed": false,
        "id": 20,
        "user_id": 2
      },
      {
        "name": "Solve this question",
        "completed": false,
        "id": 21,
        "user_id": 2
      }
    ]

3. You can make numerous such queries, exploit relationships, manage permissions and a lot more complicated stuff. Go to API Explorer on your console, and search for a query you wish to make. It will generate the entire request body for you. Also, read more about managing data  at `here <https://docs.hasura.io/0.14/manual/data/index.html>`_.

4. Your react native app with Hasura back-end is now ready. Modify it as you like.

Custom API Endpoints
^^^^^^^^^^^^^^^^^^^^

If you have a backend server ready (say in `nodejs-express` or `python-flask`)and you are looking for a place to deploy your code, it is exceptionally easy to deploy custom APIs on Hasura. Go on and read the `reference manual <https://docs.hasura.io/0.14/manual/deploying-webapp/index.html#deploy-webapp>`_.

