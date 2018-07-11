/* eslint import/prefer-default-export: 0 */

import { getElementFromAlias } from '../../../helpers/dataHelpers';

export const openAPIExplorer = () => {
  //eslint-disable-line
  // Open API Explorer
  cy.get(getElementFromAlias('api-explorer')).click();
  cy.wait(3000);
};
