/* eslint import/prefer-default-export: 0 */

import { baseUrl } from '../../../helpers/dataHelpers';

export const openAPIExplorer = () => {
  //eslint-disable-line
  // Open API Explorer
  cy.get('a')
    .contains('API Explorer')
    .click();
  cy.wait(3000);
  cy.url().should('eq', `${baseUrl}/api-explorer`);
};
