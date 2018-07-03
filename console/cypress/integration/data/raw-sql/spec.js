/* eslint import/prefer-default-export: 0 */

import { baseUrl } from '../../../helpers/dataHelpers';
import { getElementFromAlias } from '../../../helpers/dataHelpers';

export const openRawSQL = () => {
  // eslint-disable-line
  // Open RawSQL
  cy.get('a')
    .contains('Data')
    .click();
  cy.wait(3000);
  cy.get(getElementFromAlias('SQL')).click();
  cy.wait(3000);
  // Match URL
  cy.url().should('eq', `${baseUrl}/data/sql`);
  // Go back to the schema page
  cy.get('a')
    .contains('Data')
    .click();
};
