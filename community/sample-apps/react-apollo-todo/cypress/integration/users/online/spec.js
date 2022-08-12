import {
  getElementFromAlias,
  getUserName,
  baseUrl
} from "../../../helpers/dataHelpers";
import { validateTodo } from "../../validators/validators";

const userName = Cypress.env("AUTH0_USERNAME").split("@")[0];

export const checkRoute = () => {
  // Check landing page route
  cy.visit("/home");
  // wait for subscriptions to load
  cy.wait(5000);
};

export const checkOnlineUser = () => {
  cy.get(getElementFromAlias(`0_${userName}`)).contains(userName);
  cy.url().should("eq", `${baseUrl}/home`);
  //   Validate
  // validateOnlineUser(getUserName(0, userName), "success", true);
};
