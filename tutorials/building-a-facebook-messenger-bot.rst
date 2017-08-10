:orphan:

.. meta::
   :description: A tutorial on building a simple FB Messenger bot on Hasura
   :keywords: hasura, docs, tutorials, bot, chat bot, facebook, fb, messenger, fb messenger, facebook messenger
   :created-on: 2017-08-09T10:20:35.073Z

===========================================
Building a Facebook Messenger Bot on Hasura
===========================================

.. rst-class:: featured-image
.. image:: ../img/chat_tutorial_background.png
   :height: 0px
   :width: 0px

Problem
=======

* Build a bot on Facebook Messenger
* Given a movie name it should reply back with details about the movie and a poster image as well as a “More Details” button. Clicking on this button should redirect the user to a page with more details on the movie.

Architecture
============

.. image:: ../img/add_new_table_chat_profile.png

Lets get started
================

Step 1 - Hosting your server
============================

For the chat bot to function we'll need a server that will receive the messages sent by the Facebook users, process this message and respond back to the user. To send messages back the server will use the graph API provided by Facebook. For the Facebook servers to talk to our server, the endpoint URL of our server should be accessible to the Facebook server and should use a secure HTTPS callback URL. For this reason, running our server locally will not work and instead we need to host our server online. In this tutorial, we are going to deploy our server on Hasura which automatically provides SSL-enabled domains. We will use Node.js along with the express framework to build our server.
Ensure that you have Node installed on your computer, do this by running node-v in the terminal. If you do not have Node installed you can get it from `here <https://nodejs.org>`_.

-----------------------
Create a Hasura project
-----------------------

Signup or login to `Hasura <https://hasura.io/login>`_. and then head over to your `dashboard <https://dashboard.hasura.io>`_. to create a new project. Note down your project name (You will receive an email with the credentials or you could head back to your dashboard to check for the name). In this case, the project name I received was `apology69`

------------------------------
Download and install Hasuractl
------------------------------

`hasuractl` is a command line tool. You can get instructions to download and install it from `here <https://docs.hasura.io/0.14/ref/cli/hasuractl.html>`_.

Once you have installed `hasuractl`.
* Run **hasuractl login** in your terminal
* After login, run **hasuractl set-context apology69** (Replace apology69 with your project name).

-------------------------------------------
Creating your custom microservice on Hasura
-------------------------------------------

We are now going to create a microservice on Hasura where we will deploy our nodejs app. To do this :

* Run **hasuractl quickstart nodejs-express <app-name> --create**
  - Here, the <app-name> is the name you want to give to your service. In this tutorial I am going with bot. So that would be :
    + hasuractl quickstart nodejs-express bot --create
    + This creates a new service on Hasura called ‘bot’

----------------------------------------------
Deploying your code on the custom microservice
----------------------------------------------

We are now going to deploy our nodejs app onto the microservice we created in the above step.

* Add your ssh-key to the project -> Run **hasuractl add-ssh-key**
* Initialize your project to be a git repository.
  - For this, at the root directory of your project. In this case, /bot
  - Run a **git init**
  - Commit your changes -> Run **git commit -am “Initial Commit”**
* To deploy -> Run **git push hasura master**
* Navigating to bot.apology69.hasura-app.io/ will now respond with a ‘Hello World’.
* Switch back to your terminal and navigate to **/bot/app/src** and open **service.js** file using the text editor of your choosing.

.. code-block:: JavaScript

    var express = require('express');
    var app = express();

    //your routes here
    app.get('/', function (req, res) {
      res.send("Hello World!");
    });

    app.listen(8080, function () {
      console.log('Example app listening on port 8080!');
    });

As you can see, our app is listening on Port 8080, which is the default port that our microservice listens to as well.
Another thing to note is that the module 'express' is already added to our app.
* Next, Let's install additional Node dependencies.
* Go back to your terminal, navigate to **/bot/app/src**. Type **npm install request body-parser --save** and hit enter.
  - The above command installs two modules(libraries) "request" and "body-parser" into our app.
  - **request** is for sending out messages and **body-parser** is to process messages.
* Once again, open up the **server.js** and add the following at the top of your file:

.. code-block:: JavaScript

    var bodyParser = require('body-parser');
    var request = require('request');

and the following after **var app = express();**

.. code-block:: JavaScript

    // Process application/x-www-form-urlencoded
    app.use(bodyParser.urlencoded({extended: false}));

    // Process application/json
    app.use(bodyParser.json());

* Your **server.js** file should now look like so:

.. code-block:: JavaScript

    var bodyParser = require('body-parser');
    var request = require('request');
    var express = require('express');
    var app = express();

    // Process application/x-www-form-urlencoded
    app.use(bodyParser.urlencoded({extended: false}));

    // Process application/json
    app.use(bodyParser.json());

    //your routes here
    app.get('/', function (req, res) {
      res.send("Hello World!");
    });

    app.listen(8080, function () {
      console.log('Example app listening on port 8080!');
    });


Step 2 - Setting up a Facebook Application
==========================================

* Navigate to https://developers.facebook.com/apps/
* Click on **'+ Add a new app’** , give a display name for your app and a contact email.

.. image:: ../img/tutorial_fb_bot_create_fb_app_screen.png

.. image:: ../img/tutorial_fb_bot_create_fb_app_screen2.png

* In the select a product screen, hover over **Messenger** and click on **Set Up**

.. image:: ../img/tutorial_fb_bot_create_fb_app_screen3.png

-----------------
Enabling Webhooks
-----------------

* Scroll to down to the `Webhooks` section and click on the `Setup Webhooks` button.

.. image:: ../img/tutorial_fb_bot_enable_webhooks

On the pop up that comes up, we need to fill in a box with a `Callback URL` and another one with a `Verify Token`.
  - The `Callback URL` is the url that the facebook servers will hit
    + To verify our server with the `Verify Token` we give it. This will be a GET request.
    + To send the messages that our bot receives from users. This will be a POST request.

* This means that we need to create a path on our server which can be used by the facebook server to communicate to our server. To do this, switch back to your terminal and open **service.js** file.
* Paste the following code:

.. code-block:: JavaScript

    let FACEBOOK_APP_PASSWORD = 'messenger_bot_password';

    // for Facebook verification
    app.get('/webhook/', function (req, res) {
      if (req.query['hub.verify_token'] === FACEBOOK_APP_PASSWORD) {
          res.send(req.query['hub.challenge'])
      }
      res.send('Error, wrong token')
    })

    // All callbacks for Messenger will be POST-ed here
    app.post("/webhook", function (req, res) {
        console.log('Request received at webhook: ' + JSON.stringify(req.body));
        res.sendStatus(200);
    });

In the above code
  * we are choosing an arbitrary password that we will use as our `Verify Token` while `Enabling Webhooks`
  * creating a path `\webhook\` which will accept :
    - A GET request to verify the `Verify Token` being sent by the facebook servers. Incase, the token is not the same as the one we have set, we respond with an error.
    - A POST request where all of the messages that our bot receives will be posted to, by the facebook server.
        + Here, we are just printing out the received request and responding with a status code of 200.

* Let's deploy this code
  - Navigate to /bot/
  - Run **git add.**
  - Run **git commit -am "Commit message"**
  - Run **git push hasura master**

*Note: For the rest of the tutorial, when we say "Deploy your code", you need to perform the above mentioned steps.*

* Now, switch back to your facebook app page and fill in the pop up with:
  + `Callback URL` : https://bot.apology69.hasura-app.io/webhook/
  + `Verify Token` : messenger_bot_password
* Click on `Verify and save`.
