type TestMode =
  // the default one
  | 'parallel'
  // prevent all the tests from running at all
  | 'cli'
  // mentioned in the cypress/README but never used
  // TODO: remove it or fix the docs if needed
  | 'ui';

/**
 * Read and check the TEST_MODE.
 *
 * @returns {TestMode}
 */
export function getTestMode(): TestMode {
  const testMode = Cypress.env('TEST_MODE');

  if (
    typeof testMode !== 'string' ||
    (testMode !== 'parallel' && testMode !== 'cli' && testMode !== 'ui')
  ) {
    throw new Error(
      `Unexpected Cypress env variable TEST_MODE value: ${testMode}`
    );
  }

  return testMode;
}
