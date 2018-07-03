export const testMode = Cypress.env('TEST_MODE');
export const baseUrl = Cypress.env('BASE_URL');
export const migrateUrl = Cypress.env('MIGRATE_URL');
export const migrateModeUrl = `${migrateUrl}/settings`;
