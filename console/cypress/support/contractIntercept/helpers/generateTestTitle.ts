import type { TestTitle } from '../types';
import { generateDescribesTitle } from './generateDescribesTitle';

/**
 * Return a string containing all the "describe"s titles and the test title concatenated.
 *
 * Ex. return 'Describe 1 - Describe 2 - Test' in this case
 * describe('Describe 1', () => {
 *   describe('Describe 2', () => {
 *     it('Test', () => {})
 *   })
 * })
 */
export function generateTestTitle(thisTest: Mocha.Context): TestTitle {
  const describesTitle = generateDescribesTitle(thisTest.parent);
  const testTitle = thisTest.title;

  return [...describesTitle, testTitle].join(' - ');
}
