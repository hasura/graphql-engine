import type { CyHttpMessages } from 'cypress/types/net-stubbing';

// -------------------------------------------------------------------
// PACT TYPES --------------------------------------------------------
// -------------------------------------------------------------------

// Borrowed from Pact's types https://github.com/pactflow/pact-cypress-adapter/blob/main/src/types.ts
export type HeaderType = Record<string, string | string[]> | undefined;

type BaseXHR = {
  headers: HeaderType;
  body: any | undefined;
};

export type XHRRequestAndResponse = {
  request: {
    method: string;
    url: string;
  } & BaseXHR;
  response: {
    statusCode: string | number | undefined;
    // statusText: string | undefined;
  } & BaseXHR;
};

// -------------------------------------------------------------------
// CONTRACT RECORDING TYPES ------------------------------------------
// -------------------------------------------------------------------

export type TestTitle = string;
type TestPath = string;

export type ContractRequest = XHRRequestAndResponse & {
  readme: string;
  fixtureName: string;
  fixtureFileName: string;
};

export type RunningTestState = {
  testTitle: TestTitle;
  testPath: TestPath;
  halted: boolean;

  contract: ContractRequest[];
};

/**
 * Considering the current limitation of "only one test at a time can be recorded", the whole state
 * and startContractIntercept could be simplified a lot by:
 * 1. Transforming the State into a Map<Mocha.Context, RunningTestState>
 * 2. Using the test instance, instead of the test name, to store the test state
 * 3. Getting read of generateTestTitle and its tests
 *
 * Please note that this would result also in not having the test name in the cypress logs but this
 * is not important since only a single test can be recorded at a time...
 */
export type RunningTestsState = Record<TestTitle, RunningTestState>;

// -------------------------------------------------------------------
// OPTIONS TYPES -----------------------------------------------------
// -------------------------------------------------------------------

export type StartContractInterceptOptions = {
  thisTest: Mocha.Context;
  mode: 'record' | 'disabled';
  createFixtureName: (req: CyHttpMessages.IncomingHttpRequest) => string;
};
