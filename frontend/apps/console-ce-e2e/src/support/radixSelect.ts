/**
 * Pick an option of a Radix `Select` (the console's `SelectField`).
 *
 * Radix renders a visually-hidden native `<select name=...>` for form
 * submission next to the `combobox` trigger, so `cy.select()` on it fails as
 * "covered by another element". Chain this off that native select (it keeps
 * the field's `name`), and it drives the real UI instead: open the trigger,
 * then pick the option (rendered in a portal) by its label.
 *
 * The option is chosen with the keyboard: Radix selects on pointer-up
 * sequences that Cypress's synthetic `click()` doesn't reproduce, leaving the
 * popover open (and the rest of the page aria-hidden).
 *
 * @example cy.get('[name=source]').radixSelect('default')
 */
Cypress.Commands.add(
  'radixSelect',
  { prevSubject: 'element' },
  (subject, label: string) => {
    const trigger = () => cy.wrap(subject).parent().find('[role=combobox]');

    trigger().click();
    cy.findByRole('option', { name: label }).focus();
    cy.focused().type('{enter}');
    trigger()
      .should('have.attr', 'aria-expanded', 'false')
      .and('contain.text', label);
  },
);
