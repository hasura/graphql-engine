// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This is will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })
const auth0 = require("auth0-js");
import { setAuthSession } from "../helpers/common";

Cypress.Commands.add("loginAsAdmin", (overrides = {}) => {
  Cypress.log({
    name: "loginAsAdminBySingleSignOn"
  });

  const webAuth = new auth0.WebAuth({
    domain: Cypress.env("AUTH0_DOMAIN"), // Get this from https://manage.auth0.com/#/applications and your application
    clientID: Cypress.env("AUTH0_CLIENT_ID"), // Get this from https://manage.auth0.com/#/applications and your application
    responseType: "token id_token"
  });

  webAuth.client.login(
    {
      realm: "Username-Password-Authentication",
      username: Cypress.env("AUTH0_USERNAME"),
      password: Cypress.env("AUTH0_PASSWORD"),
      audience: "https://todo-hasura-test.auth0.com/api/v2/", // Get this from https://manage.auth0.com/#/apis and your api, use the identifier property
      scope: "openid email profile"
    },
    function(err, authResult) {
      // Auth tokens in the result or an error
      if (authResult && authResult.accessToken && authResult.idToken) {
        window._authResult = authResult;
        setAuthSession(authResult);
      } else {
        console.error("Problem logging into Auth0", err);
        throw err;
      }
    }
  );
});
