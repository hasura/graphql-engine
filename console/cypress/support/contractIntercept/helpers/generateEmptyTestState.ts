import type { RunningTestState } from '../types';

export function generateEmptyTestState(
  testPath: string,
  testTitle: string
): RunningTestState {
  return {
    testPath,
    testTitle,
    halted: false,
    contract: [],
  };
}
