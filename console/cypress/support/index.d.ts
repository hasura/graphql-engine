// type definition for all custom commands
declare namespace Cypress {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface Chainable<Subject> {
    /**
     * Custom command to select DOM element by data-test attribute.
     * <button data-test="greeting"> </button>
     * @example cy.getBySel('greeting')
     */
    getBySel(value: string): Chainable<Element>;
    /**
     * Custom command to select DOM element by data-test* attribute.
     * <button data-test="save_me_oh_God"> </button>
     * @example cy.getBySelLike('save_me')
     */
    getBySelLike(value: string): Chainable<Element>;
    /**
     * Custom command to work around the fact that cy.clear sometimes fails at clearing the
     * Console's textarea
     * @example cy.get('textarea').clearConsoleTextarea()
     */
    clearConsoleTextarea(): Chainable<Element>;
    /**
     * Visit the initial empty page.
     * Console's textarea
     * @example cy.visitEmptyPage()
     */
    visitEmptyPage(): Chainable<unknown>;
  }
}
