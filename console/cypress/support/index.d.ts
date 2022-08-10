// type definition for all custom commands
declare namespace Cypress {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface Chainable<Subject> {
    /**
     * Custom command to select DOM element by data-test attribute.
     * <button data-test="greeting"> </button>
     * @example cy.getBySel('greeting')
     */
    getBySel(value: string): Chainable<JQuery<Element>>;

    /**
     * Custom command to work around the fact that cy.clear sometimes fails at clearing the
     * Console's textarea
     * @example cy.get('textarea').clearConsoleTextarea()
     */
    clearConsoleTextarea(): Chainable<JQuery<HTMLTextAreaElement>>;

    /**
     * Visit the initial empty page.
     * Console's textarea
     * @example cy.visitEmptyPage()
     */
    visitEmptyPage(): Chainable<unknown>;

    /**
     * Success notifications
     */
    expectSuccessNotification(): Chainable<unknown>;
    expectSuccessNotificationWithTitle(title: string): Chainable<unknown>;
    expectSuccessNotificationWithMessage(message: string): Chainable<unknown>;

    /**
     * Error notifications
     */
    expectErrorNotification(): Chainable<unknown>;
    expectErrorNotificationWithTitle(title: string): Chainable<unknown>;
    expectErrorNotificationWithMessage(message: string): Chainable<unknown>;

    /**
     * Start intercepting the request/response contract between the Console and the server.
     * @example
     * cy.startContractIntercept(
     *   {
     *     thisTest: this.test,
     *     mode: 'record',
     *     createFixtureName: (req: CyHttpMessages.IncomingHttpRequest) => {
     *       if (req.url.endsWith('v1/metadata')) {
     *         return `v1-metadata-${req.body.type}`;
     *       }
     *
     *       throw new Error(`Unknown url ${req.url}`);
     *     },
     *   },
     *   'http://localhost:8080/**'
     * );
     */
    startContractIntercept(
      startContractInterceptOptions: import('./contractIntercept/types').StartContractInterceptOptions,
      url: string
    ): Chainable<unknown>;

    /**
     * Halt intercepting the request/response contract between the Console and the server and save the fixtures.
     * @example
     * cy.haltContractIntercept({ thisTest: this.test })
     */
    haltContractIntercept(options: {
      thisTest: Mocha.Context;
      saveFixtureFiles?: boolean;
    }): Chainable<unknown>;
  }
}
