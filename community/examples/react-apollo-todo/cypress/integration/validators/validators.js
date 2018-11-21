import { makeDataAPIOptions } from "../../helpers/dataHelpers";
// ***************** UTIL FUNCTIONS **************************

let accessKey;
let dataApiUrl;

export const setMetaData = () => {
  cy.window().then(win => {
    // accessKey = win.__env.accessKey;
    // dataApiUrl = win.__env.dataApiUrl;
    accessKey = "abcd";
    dataApiUrl = "https://hasura-todo-test.herokuapp.com";
  });
};

// ******************* VALIDATION FUNCTIONS *******************************

// ****************** Todo Validator *********************

export const validateTodo = (todoName, result, is_public) => {
  const userId = window.localStorage.getItem("auth0:id_token:sub");
  const reqBody = {
    type: "select",
    args: {
      table: "todos",
      columns: ["*"],
      where: { user_id: userId, text: todoName, is_public: is_public }
    }
  };
  const requestOptions = makeDataAPIOptions(dataApiUrl, accessKey, reqBody);
  cy.request(requestOptions).then(response => {
    console.log(response);
    if (result.status === "success") {
      console.log("inside success");
      expect(response.body.length === 1).to.be.true;
    } else if (result.status === "failure") {
      console.log("inside failure");
      expect(response.body.length === 0).to.be.true;
    }
  });
};
