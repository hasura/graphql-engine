/**
 * Clear a Console's textarea.
 * Work around cy.clear sometimes not working in the Console's textareas.
 */
Cypress.Commands.add('clearConsoleTextarea', { prevSubject: 'element' }, el => {
  cy.wrap(el).type('{selectall}', { force: true }).trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
});
