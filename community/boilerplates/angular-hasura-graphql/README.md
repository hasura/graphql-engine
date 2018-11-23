# Angular-Hasura-Boilerplate

### Angular 2+ (Working version: v7.0.2)

This repo has 3 boilerplates to get you started with Hasura and Angular 7.

- **hello-world** :

  The `hello-world` boilerplate consists of basic Angular application setup with GraphQLModule to get you started.

- **basic** :

  The `basic` boilerplate gives you an idea of how the `mutations` `subscriptions` and `queries` can be made to the Hasura GraphQL Engine.

  **What's new?**

       - Basic signin page to log you in and create sessions.
       - Examples already set up to make queries in the ```todos``` table of Hasura's Heroku deployed app.

- **advanced** :

  The `advanced` boilerplate gives you a full fledged application built on the `basic` boilerplate containing Auth0 integration.

      **What's new?**

      - Authentication using Auth0.

## Getting Started

### Installing

Clone the repo first then `cd` into the folder you want to use as a boilerplate, install all the npm packages using

```
$ npm install
```

This starts the development server at port 4200 (localhost).

### Setting up environment variables.

In each boilerplates, we are using environment file to store user and application specific data. The file `/src/environments/environment.ts` contains all the variables with the values.

To set up the env variables,

- Deploy Hasura to Heroku to recieve an endpoint that will be used to interact with your application.
- Create an access key in the Heroku application and use that same key here. This key will be used to prevent any other user access your deployed endpoint.

### Starting the server

Once all the dependencies are installed and environment variables are set, you're ready to go!

```
$ npm start
```
