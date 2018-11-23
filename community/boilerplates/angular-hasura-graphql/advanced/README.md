# Angular-Hasura-Boilerplate - advanced

## Getting Started

### Installing

You need to first install all the npm packages that are used in this boilerplate.

```
$ npm install
```

Once all the dependencies are installed, you're ready to go!

```
$ npm start
```

This starts the development server at localhost:4200.

### About this boilerplate

This boilerplate is build upon the `basic` boilerplate which contained the basic set up for the Apollo Client and examples for mutations queries and subscriptions.

In this boilerplate we'll be getting around the integrating Auth0 authentication with the basic app.

Headout to [Auth0 website](https://auth0.com/) to setup an application and get the credentials for the the client setup from there.

### Integrating your application

After setting up your application in Auth0, you are now ready to integrate your Angular application with the Auth0 application.

To get going, use the Auth0 application credentials in the file `src/environments/environment.ts` and start the application.

#### What's happening under the hood?

The `login` button fires a `login()` function defined in `AuthenticationService.ts` this redirects your application to Auth0 servers where it logs in the user and calls back to the user application with the user info like `user id` and `token`.

This `token` and `user id` are now saved in the local storage and are used to maintain sessions and the function `handleAuthentication()` in the `app.components.ts` file is called.

### Screenshots

##### Login Screen :

![login](/advanced/ss/login.png)

#

#### Dashboard :

![dash](/advanced/ss/dashboard.png)
