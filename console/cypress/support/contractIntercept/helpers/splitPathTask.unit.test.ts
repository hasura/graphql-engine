/// <reference types="Cypress" />

describe(`cy.task('splitPath')`, () => {
  it('Should split the passed path', () => {
    const path =
      'cypress/support/interceptAndRecordContract/helpers/temp.unit.test.ts';

    cy.task('splitPath', { path }).then(result => {
      expect(result).to.deep.equal([
        'cypress',
        'support',
        'interceptAndRecordContract',
        'helpers',
        'temp.unit.test.ts',
      ]);
    });
  });
});
