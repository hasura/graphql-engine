import { getElementFromAlias } from '../../helpers/dataHelpers';

export const viewOnboarding = () => {
  // Click on create
  cy.get(getElementFromAlias('onboarding-popup'))
    .should('be.visible')
    .should('contain.text', `Hi there, let's get started with Hasura!`);
  // cy.get(getElementFromAlias('btn-hide-for-now')).click();
};
export const hideNow = () => {
  // Click on create
  cy.get(getElementFromAlias('btn-hide-for-now')).click();
  cy.get(getElementFromAlias('onboarding-popup')).should('not.exist');
};

export const dontShowAgain = () => {
  // Click on create
  cy.reload();
  cy.get(getElementFromAlias('onboarding-popup')).should('be.visible');

  cy.get(getElementFromAlias('btn-ob-dont-show-again')).click();
  cy.get(getElementFromAlias('onboarding-popup')).should('not.exist');
  cy.reload();
  cy.get(getElementFromAlias('onboarding-popup')).should('not.exist');
};
