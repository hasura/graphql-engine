/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import { checkRoute, checkOnlineUser } from "./spec";
import { setMetaData } from "../../validators/validators";

const setup = () => {
  describe("Setup route", () => {
    it("Visit the index route", () => {
      // Visit the index route
      cy.visit("/home");
      cy.wait(5000);
      setMetaData();
    });
  });
};

export const runCreateTodoTests = () => {
  describe("Online Users Subscription", () => {
    beforeEach(function() {
      // runs before each test in the block to set localstorage
      cy.loginAsAdmin();
      cy.wait(5000);
    });

    it("Opens the correct route", checkRoute);
    it("Check online user subscription", checkOnlineUser);
  });
};

setup();
runCreateTodoTests();
