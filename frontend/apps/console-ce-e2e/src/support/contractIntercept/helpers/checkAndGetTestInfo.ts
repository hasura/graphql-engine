import { generateTestTitle } from './generateTestTitle';
import { throwIfCalledInTestHooks } from './throwIfCalledInTestHooks';
import { throwIfCalledInsideArrowFunction } from './throwIfCalledInsideArrowFunction';

/**
 * Perform pre-flight checks and return the most important test info.
 */
export function checkAndGetTestInfo(thisTest: Mocha.Context) {
  const testTitle = generateTestTitle(thisTest); // ex. 'Describe 1 - Describe 2 - Describe 3 - Test title'
  const testPath = thisTest?.invocationDetails?.relativeFile;

  throwIfCalledInTestHooks(thisTest.title);
  throwIfCalledInsideArrowFunction(thisTest.title);

  // TS-only check, it should never happen at runtime
  if (typeof testPath !== 'string') {
    throw new Error(`No test Path available for ${testTitle}`);
  }

  return {
    testPath,
    testTitle,
  };
}
