---
title: "Create Auth0 App"
metaTitle: "Create Auth0 App | Hasura GraphQL Tutorial"
metaDescription: "In this part, we will learn how to create Auth0 app using the dashboard for a Single Page Web Application."
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/BXJk0wao42U" />

1. Navigate to the [Auth0 Dashboard](https://manage.auth0.com/)
2. Signup / Login to the account
3. Create a new tenant.
4. Click on the `Applications` menu option on the left and then click the `+ Create Application` button.
5. In the Create Application window, set a name for your application and select `Single Page Web Applications`. (Assuming the frontend app will be an SPA built on react/vue etc)

![Create Auth0 App](https://graphql-engine-cdn.hasura.io/learn-hasura/assets/graphql-hasura/create-auth0-app.png)

6. In the settings of the application, we will add appropriate (e.g: http://localhost:3000/callback) URLs as Allowed Callback URLs and Allowed Web Origins. We can also add domain specific URLs as well for the app to work. (e.g: https://myapp.com/callback). 

This would be the URL of the frontend app which you will deploy later. You can ignore this, for now. You can always come back later and add the necessary URLs.

