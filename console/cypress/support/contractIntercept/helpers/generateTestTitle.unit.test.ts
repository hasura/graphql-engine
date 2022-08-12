/// <reference types="Cypress" />

import { generateTestTitle } from './generateTestTitle';

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

type SimplifiedTestSuite = SimplifiedTestOrDescribe;
// -------------------------------------------------------------------
// -------------------------------------------------------------------
// -------------------------------------------------------------------

describe('generateTestTitle', () => {
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
    // testSuite is compatible for what concerns to generateTestTitle that will only look for the
    // `parent` and `title` properties
    const result = generateTestTitle((testSuite as unknown) as Mocha.Context);

    // Assert
    expect(result).to.deep.equal(
      'Describe 1 - Describe 2 - Describe 3 - Test title'
    );
  });
});
