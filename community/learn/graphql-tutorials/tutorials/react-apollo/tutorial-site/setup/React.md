Great! Now you are done learning about major concepts of GraphQL and how it can be useful for development. So let's get started on the application that we are going to build. 

Without wasting any more time, we can directly jump into the GraphQL code as we already have the boilerplate UI ready [here](https://github.com/hasura/react-apollo-todo/tree/master/react-apollo/ui-boilerplate). You can clone it and start your GraphQL journery immediatedly.

This tutorial uses `create-react-app` with a few other essential modules for styles (`react-bootstrap`) , routing (`react-router`) and workflow tooling (`eslint`, `prettier`, `prop-types`). And for authentication we use `Auth0`.

The backend is already built and available [here](https://react-apollo-todo-demo.hasura.app/v1alpha1/graphql). You will make use of this for testing and building your frontend app with graphql integration.

Once you have cloned the boilerplate, navigate to `src/utils/constants.js` to look at the values of GraphQL Endpoint.

Here we are setting up values to be used inside our todo app. Since we have our backend ready (powered by Hasura), we will use the graphql endpoint and the realtime websocket endpoint.

#### Directory Structure

    This is the directory structure of the boilerplate that you cloned.

    src
      - components
      - images
      - styles
      - utils
      - routes.js
      - apollo.js
      - index.js
    public
      - index.html
    package.json

#### Finished Application

Though you are going to follow the tutorial and write actual code to complete the app, you can always refer the already finished app code [here](https://github.com/hasura/react-apollo-todo/tree/master/react-apollo/final-app), in case you are stuck anywhere or you want to quickly refer to a code snippet.
