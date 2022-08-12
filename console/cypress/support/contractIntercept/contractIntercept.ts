import type {
  ContractRequest,
  RunningTestsState,
  StartContractInterceptOptions,
} from './types';

import { checkAndGetTestInfo } from './helpers/checkAndGetTestInfo';
import { generateEmptyTestState } from './helpers/generateEmptyTestState';

let runningTestState: RunningTestsState = {};

/**
 * A wrapper around `cy.intercept` that allows intercepting and recording the request/response series
 * that made up the contract.
 *
 * It could be useful for sharing the contract's fixtures with the server folks.
 *
 * Be aware of the current limitations:
 * 1. Only one test at the time is supported. `runningTestState` can store more tests at once but
 * the fixture files are all saved starting from "1 - ", even if related to different tests
 * 2. If you do not call cy.haltContractIntercept, no fixture files will be saved
 *
 * @see https://github.com/hasura/graphql-engine-mono/issues/4601
 */
function startContractIntercept(
  startContractInterceptOptions: StartContractInterceptOptions,
  url: string
) {
  const { thisTest, mode, createFixtureName } = startContractInterceptOptions;
  const { testTitle, testPath } = checkAndGetTestInfo(thisTest);

  if (mode === 'disabled') {
    Cypress.log({
      message: `*🤝 ❌ No Contract will be recorded for ${testTitle}*`,
    });
    return;
  }

  Cypress.log({
    message: `*🤝 ✅ Contract will be recorded for ${testTitle}*`,
  });

  runningTestState[testTitle] ??= generateEmptyTestState(testPath, testTitle);

  if (Object.keys(runningTestState).length > 1) {
    throw new Error(`startContractIntercept support only one test at a time`);
  }
  // Start intercepting the requests
  cy.intercept(url, request => {
    // The recorded could have been halted
    if (runningTestState[testTitle].halted) {
      Cypress.log({
        message: `*🤝 ❌ Contract recording has been halted for: ${testTitle}*`,
      });
      return;
    }

    const fixtureName = createFixtureName(request);
    if (fixtureName.includes('\\') || fixtureName.includes('/')) {
      throw new Error(
        `createFixtureName cannot return names that includes / or \ like ${fixtureName}`
      );
    }

    const contractLength = runningTestState[testTitle].contract.length;

    // start from 1
    const fixtureIndex = contractLength + 1;
    const fixtureFileName = `${fixtureIndex}-${fixtureName}.json`;

    const recorded: ContractRequest = {
      readme:
        '////////// This fixture has been automatically generated through cy.startContractIntercept //////////',
      request,
      fixtureName,
      fixtureFileName,

      // Temporary, empty, response
      response: {
        statusCode: undefined,
        headers: undefined,
        body: undefined,
      },
    };
    // Add the request to the Contract
    runningTestState[testTitle].contract.push(recorded);

    Cypress.log({
      message: `*🤝 ✅ Recorded ${fixtureFileName} in the contract*`,
      consoleProps: () => request,
    });

    request.continue(response => {
      // Add the request to the Contract too
      recorded.response = response;
    });
  });
}

/**
 * Halt recording the contract and save the fixture files.
 * Please note that it must be called just once
 */
function haltContractIntercept(options: {
  thisTest: Mocha.Context;
  saveFixtureFiles?: boolean;
}) {
  const { thisTest, saveFixtureFiles = true } = options;
  const { testTitle, testPath } = checkAndGetTestInfo(thisTest);

  if (!saveFixtureFiles) {
    Cypress.log({
      message: `*🤝 ❌ No fixtures will be saved for this test: ${testTitle}*`,
    });
    return;
  }

  if (runningTestState[testTitle].halted) {
    Cypress.log({
      message: `*🤝 ❌ Contract recording for this test has already been halted: ${testTitle}*`,
    });
  }

  // Halt recording the requests for the current test.
  // Please note that must be done asynchronously because of the double-run nature of the Cypress tests.
  cy.wrap(null).then(() => {
    Cypress.log({
      message: `*🤝 ❌ Halting the contract recording for this test: ${testTitle}*`,
    });
    runningTestState[testTitle].halted = true;
  });

  // Split the current path
  cy.task('splitPath', { path: testPath }).then(result => {
    const splittedPath = result as string[];

    // Remove the file name
    splittedPath.pop();

    // Create the directory
    cy.task('joinPath', { path: [...splittedPath, 'fixtures'] }).then(path => {
      cy.task('mkdirSync', {
        dir: path as string,
      });
    });

    const testState = runningTestState[testTitle];

    // Save all the files
    for (let i = 0, n = testState.contract.length; i < n; i++) {
      const request = testState.contract[i];

      cy.task('joinPath', {
        // Stores the fixture files close to the test file, in a "fixtures" directory
        path: [...splittedPath, 'fixtures', request.fixtureFileName],
      }).then(filePath => {
        // Save the fixture file
        cy.task('writeFileSync', {
          file: filePath as string,
          data: JSON.stringify(request, null, 2),
        });
      });
    }
  });
}

Cypress.Commands.add('startContractIntercept', startContractIntercept);
Cypress.Commands.add('haltContractIntercept', haltContractIntercept);
