/* eslint import/prefer-default-export: 0 */

import { validateMigrationMode } from '../../validators/validators';

import { toggleOnMigrationMode, toggleOffMigrationMode } from './utils';

export const checkToggleButton = () => {
  // eslint-disable-line
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
};
