import {
  getElementFromAlias,
  getTodoName,
  baseUrl
} from "../../../helpers/dataHelpers";
import { validateTodo } from "../../validators/validators";

const testName = "publictodo";

export const checkRoute = () => {
  // Check landing page route
  cy.visit("/home");
  // wait for subscriptions to load
  cy.wait(5000);
};

export const createTodo = () => {
  cy.get(getElementFromAlias("input-public"))
    .clear()
    .type(getTodoName(0, testName))
    .type("{enter}");
  cy.url().should("eq", `${baseUrl}/home`);
  //  Check if the todo got created
  cy.get(getElementFromAlias(`public_0_${getTodoName(0, testName)}`)).contains(
    getTodoName(0, testName)
  );
  //   Validate
  validateTodo(getTodoName(0, testName), "success", true);
};

export const deleteTodo = () => {
  cy.url().should("eq", `${baseUrl}/home`);
  //   Click on delete
  cy.get(
    getElementFromAlias(`remove_public_0_${getTodoName(0, testName)}`)
  ).click();
  cy.wait(2000);
  cy.get(getElementFromAlias(`public_0_${getTodoName(0, testName)}`)).should(
    "not.exist"
  );
  //   Validate
  validateTodo(getTodoName(0, testName), "failure", true);
};
