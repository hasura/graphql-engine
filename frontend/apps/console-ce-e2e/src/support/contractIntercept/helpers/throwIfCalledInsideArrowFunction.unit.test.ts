/// <reference types="Cypress" />

import { throwIfCalledInsideArrowFunction } from './throwIfCalledInsideArrowFunction';

describe('throwIfCalledInsideArrowFunction', () => {
  it('Should throw an error if called without a the reference to the `this.test`', () => {
    // Arrange
    const executor = () => throwIfCalledInsideArrowFunction({});

    // Assert
    expect(executor).to.throw(
      'interceptAndRecordContract did not receive `this` that refers to the test itself. Have you called interceptAndRecordContract inside an arrow function? If yes, transform function into a regular one'
    );
  });
});
