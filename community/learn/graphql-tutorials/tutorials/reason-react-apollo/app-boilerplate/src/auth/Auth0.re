[@bs.val] external windowOrigin : string = "window.location.origin";

// auth0.re
type generatedAuth0Client = {.
  "authorize": [@bs.meth] (unit => unit),
  "isAuthenticated": [@bs.meth] (bool => unit)
};

type clientOptions = {
  .
  "domain": string,
  "clientID": string,
  "redirectUri": string,
  "responseType": string,
  "scope": string,
};

let options = {
  "domain": "graphql-tutorials.auth0.com",
  "clientID": "P38qnFo1lFAQJrzkun--wEzqljVNGcWW",
  "redirectUri": windowOrigin ++ "/callback",
  "responseType": "token id_token",
  "scope": "openid"
};

[@bs.module "auth0-js"] [@bs.new]  external createClient : (clientOptions => generatedAuth0Client) = "WebAuth";
