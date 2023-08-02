// type definition for all custom commands
declare namespace Cypress {
  interface PluginConfig {
    useRelativeSnapshots: boolean;
  }
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface Chainable<Subject> {
    /**
     * Custom command to work around the fact that cy.clear sometimes fails at clearing the
     * Console's textarea
     * @example cy.get('textarea').clearConsoleTextarea()
     */
    clearConsoleTextarea(): Chainable<JQuery<HTMLTextAreaElement>>;

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

    toMatchSnapshot(options?: { name?: string }): void;
  }
}
