---
title: "Deploy Hasura"
metaTitle: "Deploy Hasura to Heroku | Hasura GraphQL Tutorial"
metaDescription: "This tutorial covers how to deploy Hasura GraphQL Engine on Heroku using one-click deployment and access the Hasura Console"
---

import YoutubeEmbed from "../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/yOVHEzUiH84" />

Let's start by deploying Hasura.

## One-click deployment on Heroku

The fastest way to try Hasura out is via Heroku.

- Click on the following button to deploy GraphQL Engine on Heroku with the free Postgres add-on:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

    This will deploy Hasura GraphQL Engine on Heroku. A PostgreSQL database will be automatically provisioned along with Hasura. If you don’t have an account on Heroku, you would be required to sign up. 
    *Note*: It is free to signup and no credit-card is required.

![Deploy on Heroku](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/deploy-on-heroku.png)

Type in the app name, select the region of choice and click on Deploy app button.

## Hasura Console

Once the app is deployed, you should see the following on your Heroku dashboard.

![Hasura GraphQL on Heroku](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/app-deployed-heroku.png)

- Open the Hasura console

    Click on the `View` button to open the app. 
    Alternatively you can always visit `https://<app-name>.herokuapp.com` (*replace \<app-name\> with your app name*) to open the admin console.

It should look something like this:

![Hasura Console](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/hasura-console-initial-load.png)

Great! You have now deployed Hasura GraphQL Engine and have the admin console ready to get started!
