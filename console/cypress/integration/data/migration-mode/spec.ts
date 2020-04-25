import { validateMigrationMode } from '../../validators/validators';

import { toggleOnMigrationMode, toggleOffMigrationMode } from './utils';
import { getElementFromAlias } from '../../../helpers/dataHelpers';

export const testToggleButton = () => {
  // Turn off migration mode
  toggleOffMigrationMode();
  cy.wait(10000);
  // Validate
  validateMigrationMode(false);
  cy.wait(7000);
  // Turn on migration mode
  toggleOnMigrationMode();
  cy.wait(10000);
  // Validate
  validateMigrationMode(true);
  cy.wait(7000);
  cy.get(getElementFromAlias('schema')).click();
};

export const checkToggleButton = () => {
  cy.window().then(win => {
    // eslint-disable-next-line no-underscore-dangle
    const { consoleMode } = win.__env;
    if (consoleMode === 'cli') {
      testToggleButton();
    } else {
      cy.get('[class=react-toggle-track]').should('not.exist');
    }
  });
};
