export const baseUrl = Cypress.config("baseUrl");
export const getElementFromAlias = alias => `[data-test=${alias}]`;
export const getTodoName = (i, todoName = "") =>
  `tutorial_test_todo_${todoName}_${i}`;
export const getUserName = (i, userName = "") => `${i}_${userName}`;
export const makeDataAPIUrl = dataApiUrl => `${dataApiUrl}/v1/query`;
export const makeDataAPIOptions = (dataApiUrl, key, body) => ({
  method: "POST",
  url: makeDataAPIUrl(dataApiUrl),
  headers: {
    "x-hasura-access-key": key
  },
  body,
  failOnStatusCode: false
});
