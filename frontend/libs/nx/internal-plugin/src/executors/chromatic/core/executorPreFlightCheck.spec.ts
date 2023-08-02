import produce from 'immer';

import type {
  PreFlightCheckParams,
  PreFlightCheckResult,
} from './executorPreFlightCheck';

import { executorPreFlightCheck } from './executorPreFlightCheck';

const happyPathParams: PreFlightCheckParams = {
  executorContextProjectName: {
    projectName: 'legacy-ce',
    projectConfiguration: {
      root: 'libs/console/legacy-ce',
      targets: {
        'build-storybook': {
          executor: '@nrwl/storybook:build',
          options: {
            outputPath: 'dist/storybook/console/legacy-ce',
          },
        },
      },
    },
  },
  envVars: {
    BUILDKITE: 'true',
    BUILDKITE_PULL_REQUEST: '9999',
  },
};

describe('executorPreFlightCheck', () => {
  it('When passed with a valid configuration, it returns we are on the pr branch', () => {
    const expected: PreFlightCheckResult = {
      mode: 'pr',
      prNumber: 9999,
      distTarget: 'dist/storybook/console/legacy-ce',
    };

    const result = executorPreFlightCheck(happyPathParams);
    expect(result).toEqual(expected);
  });

  describe('Env vars combinations', () => {
    describe('BUILDKITE_PULL_REQUEST', () => {
      it('When passed with a number for BUILDKITE_PULL_REQUEST, it returns we are on the pr branch', () => {
        const params = produce(happyPathParams, draft => {
          // @ts-expect-error Trying every crazy possible env var value
          draft.envVars.BUILDKITE_PULL_REQUEST = 9999;
        });

        const expected: PreFlightCheckResult = {
          mode: 'pr',
          prNumber: 9999,
          distTarget: 'dist/storybook/console/legacy-ce',
        };

        const result = executorPreFlightCheck(params);
        expect(result).toEqual(expected);
      });

      it('When passed with an empty BUILDKITE_PULL_REQUEST, it returns we are on the main branch', () => {
        const params = produce(happyPathParams, draft => {
          draft.envVars.BUILDKITE_PULL_REQUEST = '';
        });

        const expected: PreFlightCheckResult = {
          mode: 'mainBranch',
          distTarget: 'dist/storybook/console/legacy-ce',
        };

        const result = executorPreFlightCheck(params);
        expect(result).toEqual(expected);
      });

      it('When passed with an undefined BUILDKITE_PULL_REQUEST, it returns we are on the main branch', () => {
        const params = produce(happyPathParams, draft => {
          draft.envVars.BUILDKITE_PULL_REQUEST = undefined;
        });

        const expected: PreFlightCheckResult = {
          mode: 'mainBranch',
          distTarget: 'dist/storybook/console/legacy-ce',
        };

        const result = executorPreFlightCheck(params);
        expect(result).toEqual(expected);
      });

      it('When passed with a "false" BUILDKITE_PULL_REQUEST, it returns we are on the main branch', () => {
        const params = produce(happyPathParams, draft => {
          draft.envVars.BUILDKITE_PULL_REQUEST = 'false';
        });

        const expected: PreFlightCheckResult = {
          mode: 'mainBranch',
          distTarget: 'dist/storybook/console/legacy-ce',
        };

        const result = executorPreFlightCheck(params);
        expect(result).toEqual(expected);
      });
    });

    describe('BUILDKITE', () => {
      it('When passed with an empty BUILDKITE, it returns we are on the main branch', () => {
        const params = produce(happyPathParams, draft => {
          draft.envVars.BUILDKITE = '';
        });

        expect(() => executorPreFlightCheck(params)).toThrow(
          new Error('Chromatic executor should only been run in CI')
        );
      });

      it('When passed with an undefined BUILDKITE, it returns we are on the main branch', () => {
        const params = produce(happyPathParams, draft => {
          draft.envVars.BUILDKITE = undefined;
        });

        expect(() => executorPreFlightCheck(params)).toThrow(
          new Error('Chromatic executor should only been run in CI')
        );
      });
    });
  });

  it('When the projectName is empty, it must throw', () => {
    const params = produce(happyPathParams, draft => {
      draft.executorContextProjectName.projectName = '';
    });

    expect(() => executorPreFlightCheck(params)).toThrow(
      new Error(`Unexpected project name `)
    );
  });

  it('When the outputPath is missing, it must return a default value', () => {
    const params = produce(happyPathParams, draft => {
      if (draft.executorContextProjectName.projectConfiguration)
        draft.executorContextProjectName.projectConfiguration.targets = {};
    });

    const expected: PreFlightCheckResult = {
      mode: 'pr',
      prNumber: 9999,
      distTarget: `dist/storybook/console/legacy-ce`,
    };

    const result = executorPreFlightCheck(params);
    expect(result).toEqual(expected);
  });
});
