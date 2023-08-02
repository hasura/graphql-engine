/// <reference types="Cypress" />

export function getBaseUrl() {
  // ex. http://localhost:4200/
  const cypressBaseUrl = Cypress.config('baseUrl');

  if (!cypressBaseUrl)
    throw new Error(`Cypress baseUrl is empty ${cypressBaseUrl}`);

  // ex. http://localhost:4200
  const baseUrl = cypressBaseUrl.endsWith('/')
    ? cypressBaseUrl.slice(0, -1)
    : cypressBaseUrl;

  return baseUrl;
}
