/// <reference types="Cypress" />

import { throwIfCalledInTestHooks } from './throwIfCalledInTestHooks';

describe('throwIfCalledInTestHooks', () => {
  it('Should throw an error if called with "after" hook', () => {
    // Arrange
    const executor = () => throwIfCalledInTestHooks('"after" hook');

    // Assert
    expect(executor).to.throw(
      'interceptAndRecordContract cannot be called inside a "after" hook'
    );
  });

  it('Should throw an error if called with "after all" hook', () => {
    // Arrange
    const executor = () => throwIfCalledInTestHooks('"after all" hook');

    // Assert
    expect(executor).to.throw(
      'interceptAndRecordContract cannot be called inside a "after all" hook'
    );
  });

  it('Should throw an error if called with "before" hook', () => {
    // Arrange
    const executor = () => throwIfCalledInTestHooks('"before" hook');

    // Assert
    expect(executor).to.throw(
      'interceptAndRecordContract cannot be called inside a "before" hook'
    );
  });

  it('Should throw an error if called with "before all" hook', () => {
    // Arrange
    const executor = () => throwIfCalledInTestHooks('"before all" hook');

    // Assert
    expect(executor).to.throw(
      'interceptAndRecordContract cannot be called inside a "before all" hook'
    );
  });

  it('Should not throw an error if called with another test title', () => {
    // Arrange
    const executor = () => throwIfCalledInTestHooks('Test title');

    // Assert
    expect(executor).not.to.throw(
      'interceptAndRecordContract cannot be called inside a "before all" hook'
    );
  });
});
