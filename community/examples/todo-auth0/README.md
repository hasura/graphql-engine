# Todo app using Hasura GraphQL Engine, Heroku Postgres and Auth0

* STEP 1: Set auth0 domain

  Set your auth0 domain in `auth-webhook/constants.js`.
  Also, allow callbacks for `http://localhost:3000/callback` in your `auth0` dashboard.

* STEP 2: Deploy the auth Webhook (using ngrok, glitch, Heroku or whatever)

  [![glitch-deploy-button](https://raw.githubusercontent.com/hasura/sample-auth-webhook/master/assets/deploy-glitch.png)](https://glitch.com/edit/#!/thundering-brick)

* STEP 3: Start GraphQL engine with the auth-hook as the webhook URL and access key of your choice

  Deploy GraphQL engine to Heroku if you do not have it deployed anywhere.

  [![Deploy to heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

* STEP 4: Apply the migrations

  - Add your database URL and access key in `hasura-graphql-engine/config.yaml`
  - Run `hasura migrate apply` to create the required tables and permissions for the todo app

* Step 5: Set React app variables

  Set `auth0 domain`, `auth0 client ID` and the `GraphQL Engine URL` in `todo-app/src/constants.js`

* Step 6: Run the React app

  Run `npm start` from the `todo-app` directory to start the TODO app.

  > The app runs on port 3000 by default. You can change the port number, but you will also have to reconfigure the callback.

