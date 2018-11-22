export const testMode = Cypress.env("TEST_MODE");
export const baseUrl = Cypress.config("baseUrl");

export const setAuthSession = authResult => {
  let expiresAt = JSON.stringify(
    authResult.expiresIn * 1000 + new Date().getTime()
  );
  var base64Url = authResult.idToken.split(".")[1];
  var base64 = base64Url.replace(/-/g, "+").replace(/_/g, "/");
  const decodedJwt = JSON.parse(window.atob(base64));
  const sub = decodedJwt.sub;
  window.localStorage.setItem("auth0:access_token", authResult.accessToken);
  window.localStorage.setItem("auth0:id_token", authResult.idToken);
  window.localStorage.setItem("auth0:expires_at", expiresAt);
  window.localStorage.setItem("auth0:id_token:sub", sub);
};
