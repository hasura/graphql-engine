/**
 * Throw if interceptAndRecordContract has been called without the `this.test` element.
 *
 * It happens when the test is called inside an arrow function
 * ex.
 * it('...', () => { // <- THIS is the problem
 *   cy.startContractIntercept({
 *     thisTest: this.test,
 *     // ...
 */
export function throwIfCalledInsideArrowFunction(thisTest: Mocha.Context | {}) {
  if (Object.keys(thisTest).length === 0) {
    throw new Error(
      'interceptAndRecordContract did not receive `this` that refers to the test itself. Have you called interceptAndRecordContract inside an arrow function? If yes, transform function into a regular one'
    );
  }
}
