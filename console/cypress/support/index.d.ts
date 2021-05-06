// type definition for all custom commands
declare namespace Cypress {
  interface Chainable<Subject> {
    /**
     * Custom command to get one or more DOM elements by data-test attribute.
     * <button data-test="greeting"> </button>
     * @example
     *    cy.getBySel('greeting')
     */
    getBySel(
      selector: string,
      options?: Partial<Loggable & Timeoutable & Withinable>
    ): Chainable<Element>;

    /**
     * Custom command to get one or more DOM elements by selector by data-test* attribute.
     * <button data-test="save_me_oh_God"> </button>
     * @example
     *    cy.getBySelLike('save_me')
     */
    getBySelLike(
      selector: string,
      options?: Partial<Loggable & Timeoutable & Withinable>
    ): Chainable<Element>;

    /**
     * Custom command to check if notification with title was displayed.
     * @example
     *    cy.checkNotification('Relationship renamed', {timeout: 5000}); // type defaults to success types
     *    cy.checkNotification('Relationship renamed'); // options are optional
     *    cy.checkNotification('Error adding relationship!', { timeout: 10000, type: 'error',});
     */
    checkNotification(
      content: string,
      options?: {
        type?: 'error' | 'success' | 'info';
        timeout?: number;
      }
    ): Cypress.Chainable<JQuery<HTMLElement>>;

    /**
     * Custom command to stun wwindow.prompt.
     * @param value - Value to fill the prompt with
     * @param callback - a callback function with subsequent assertions
     * @example
     *    cy.setPrompt('users-table', () => {
     *        cy.getBySel('delete-table').click();
     *        cy.window().its('prompt').should('be.called');
     *        cy.checkNotification('Relationship renamed', {timeout: 5000});
     *        validateCT('users-table', ResultType.FAILURE);
     *    });
     */
    setPrompt(value: string, callback?: () => void): void;
  }
}
