/// <reference types="Cypress" />

import { generateDescribesTitle } from './generateDescribesTitle';

// -------------------------------------------------------------------
// TYPES -------------------------------------------------------------
// -------------------------------------------------------------------
// This is a simplified version of Mocha.Suite
type SimplifiedTestRoot = {
  title: string;
};

type SimplifiedTestOrDescribe = {
  parent: SimplifiedTestRoot | SimplifiedTestOrDescribe;
  title: string;
};

type SimplifiedTestSuite = SimplifiedTestOrDescribe | SimplifiedTestRoot;
// -------------------------------------------------------------------
// -------------------------------------------------------------------
// -------------------------------------------------------------------

describe('generateDescribesTitle', () => {
  it('Should return an array including all the titles of the chain of test.describe and the test itself', () => {
    // Arrange
    // In case of a test file like the following
    // describe('Describe 1', () => {
    //   describe('Describe 2', () => {
    //     describe('Describe 3', () => {
    //       it('Test title', () => {})
    //     })
    //   })
    // })
    const testSuite: SimplifiedTestSuite = {
      parent: {
        parent: {
          parent: {
            title: 'Describe 1',
          },
          title: 'Describe 2',
        },
        title: 'Describe 3',
      },
      title: 'Test title',
    };

    // Act
    // testSuite is compatible for what concerns to generateDescribesTitle that will only look for the
    // `parent` and `title` properties
    const result = generateDescribesTitle(testSuite.parent as Mocha.Suite);

    // Assert
    expect(result).to.deep.equal(['Describe 1', 'Describe 2', 'Describe 3']);
  });

  it('Should return an array including only the test title in case of no describes', () => {
    // Act
    // testSuite is compatible for what concerns to generateDescribesTitle that will only look for the
    // `parent` and `title` properties
    const result = generateDescribesTitle(undefined);

    // Assert
    expect(result).to.deep.equal([]);
  });

  it('Should return an array including all the titles of the chain of test.describe and the test hook itself', () => {
    // Arrange
    // In case of a test file like the following
    // describe('Describe 1', () => {
    //   before(() => {})
    //   it('Test title', () => {})
    // })
    const testSuite: SimplifiedTestSuite = {
      parent: {
        title: 'Describe 1',
      },
      title: '"before" hook',
    };

    // Act
    // testSuite is compatible for what concerns to generateDescribesTitle that will only look for the
    // `parent` and `title` properties
    const result = generateDescribesTitle(testSuite.parent as Mocha.Suite);

    // Assert
    expect(result).to.deep.equal(['Describe 1']);
  });
});
