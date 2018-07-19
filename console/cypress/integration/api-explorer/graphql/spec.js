/* eslint import/prefer-default-export: 0 */

import { getElementFromAlias } from '../../../helpers/dataHelpers';

export const openAPIExplorer = () => {
  //eslint-disable-line
  // Open API Explorer
  cy.get(getElementFromAlias('api-explorer')).click();
  cy.wait(3000);
};

export const checkExecuteQueryButton = () => {
  cy.get('.execute-button');
  cy.get('.cm-def').contains('errors');
  cy.get('textarea')
    .first()
    .type('{enter}{uparrow}query', { force: true });
};
