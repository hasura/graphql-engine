import {
  transformStatelogToCLILog,
  getCliProgressState,
  getSampleQueriesUrl,
} from './util';
import {
  OneClickDeploymentState,
  OneClickDeploymentStateTransition,
  CliLog,
  ProgressState,
} from './types';

const tc: {
  name: string;
  input: OneClickDeploymentStateTransition[];
  output: ProgressState;
  only?: boolean;
}[] = [
  {
    name: 'loading state works as expected with multiple state transition',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
    ],
    output: {
      [OneClickDeploymentState.Initialized]: { kind: 'success' },
      [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
      [OneClickDeploymentState.ReadingEnvironmentVariables]: {
        kind: 'in-progress',
      },
      [OneClickDeploymentState.AwaitingEnvironmentVariables]: { kind: 'idle' },
      [OneClickDeploymentState.SufficientEnvironmentVariables]: {
        kind: 'idle',
      },
      [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
        kind: 'idle',
      },
      [OneClickDeploymentState.Completed]: { kind: 'idle' },
    },
  },
  {
    name: 'error state works as expected',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: { message: 'unexpected' },
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.Error,
      },
    ],
    output: {
      [OneClickDeploymentState.Initialized]: { kind: 'success' },
      [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
      [OneClickDeploymentState.ReadingEnvironmentVariables]: {
        kind: 'error',
        error: { message: 'unexpected' },
        logId: 2,
      },
      [OneClickDeploymentState.AwaitingEnvironmentVariables]: { kind: 'idle' },
      [OneClickDeploymentState.SufficientEnvironmentVariables]: {
        kind: 'idle',
      },
      [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
        kind: 'idle',
      },
      [OneClickDeploymentState.Completed]: { kind: 'idle' },
    },
  },
  {
    name: 'new workflows get more preference, success steps are reused',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: { message: 'unexpected' },
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.Error,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Error,
        to_state: OneClickDeploymentState.Initialized,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.SufficientEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.SufficientEnvironmentVariables,
        to_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
      },
      {
        id: 2,
        additional_info: { message: 'unable to connect' },
        from_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
        to_state: OneClickDeploymentState.Error,
      },
    ],
    output: {
      [OneClickDeploymentState.Initialized]: { kind: 'success' },
      [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
      [OneClickDeploymentState.ReadingEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.AwaitingEnvironmentVariables]: { kind: 'idle' },
      [OneClickDeploymentState.SufficientEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
        kind: 'error',
        error: { message: 'unable to connect' },
        logId: 2,
      },
      [OneClickDeploymentState.Completed]: { kind: 'idle' },
    },
  },
  {
    name: 'awaiting env state works as expected',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: [{ Kind: 'ENV_TYPE_STATIC', Name: 'PG_URL' }],
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.AwaitingEnvironmentVariables,
      },
    ],
    output: {
      [OneClickDeploymentState.Initialized]: { kind: 'success' },
      [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
      [OneClickDeploymentState.ReadingEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.AwaitingEnvironmentVariables]: {
        kind: 'awaiting',
        payload: [{ Kind: 'ENV_TYPE_STATIC', Name: 'PG_URL' }],
      },
      [OneClickDeploymentState.SufficientEnvironmentVariables]: {
        kind: 'idle',
      },
      [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
        kind: 'idle',
      },
      [OneClickDeploymentState.Completed]: { kind: 'idle' },
    },
  },
  {
    name: 'completed state works as expected',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: { message: 'unexpected' },
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.Error,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Error,
        to_state: OneClickDeploymentState.Initialized,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.SufficientEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.SufficientEnvironmentVariables,
        to_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
        to_state: OneClickDeploymentState.Completed,
      },
    ],
    output: {
      [OneClickDeploymentState.Initialized]: { kind: 'success' },
      [OneClickDeploymentState.CloningGitRepository]: { kind: 'success' },
      [OneClickDeploymentState.ReadingEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.AwaitingEnvironmentVariables]: { kind: 'idle' },
      [OneClickDeploymentState.SufficientEnvironmentVariables]: {
        kind: 'success',
      },
      [OneClickDeploymentState.ApplyingMetadataMigrationsSeeds]: {
        kind: 'success',
      },
      [OneClickDeploymentState.Completed]: { kind: 'success' },
    },
  },
];

const testCases: {
  name: string;
  input: OneClickDeploymentStateTransition[];
  output: CliLog[];
  only?: boolean;
}[] = [
  {
    name: 'loading state works as expected with multiple state transition',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
    ],
    output: [
      {
        kind: 'success',
        step: OneClickDeploymentState.Initialized,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.CloningGitRepository,
      },
      {
        kind: 'loading',
        step: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
    ],
  },
  {
    name: 'error state works as expected',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: { message: 'unexpected' },
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.Error,
      },
    ],
    output: [
      {
        kind: 'success',
        step: OneClickDeploymentState.Initialized,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.CloningGitRepository,
      },
      {
        kind: 'error',
        step: OneClickDeploymentState.ReadingEnvironmentVariables,
        payload: { message: 'unexpected' },
      },
    ],
  },
  {
    name: 'only the new workflow is shown',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: { message: 'unexpected' },
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.Error,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Error,
        to_state: OneClickDeploymentState.Initialized,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
      },
      {
        id: 2,
        additional_info: { message: 'unable to connect' },
        from_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
        to_state: OneClickDeploymentState.Error,
      },
    ],
    output: [
      {
        kind: 'success',
        step: OneClickDeploymentState.Initialized,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.CloningGitRepository,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        kind: 'error',
        step: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
        payload: { message: 'unable to connect' },
      },
    ],
  },
  {
    name: 'awaiting env state works as expected',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: [{ Kind: 'ENV_TYPE_STATIC', Name: 'PG_URL' }],
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.AwaitingEnvironmentVariables,
      },
    ],
    output: [
      {
        kind: 'success',
        step: OneClickDeploymentState.Initialized,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.CloningGitRepository,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        kind: 'awaiting-env',
        step: OneClickDeploymentState.AwaitingEnvironmentVariables,
        payload: [
          {
            Kind: 'ENV_TYPE_STATIC',
            Name: 'PG_URL',
          },
        ],
      },
    ],
  },
  {
    name: 'completed state works as expected',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.Completed,
      },
    ],
    output: [
      {
        kind: 'success',
        step: OneClickDeploymentState.Initialized,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.CloningGitRepository,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.Completed,
      },
    ],
  },
  {
    name: 'transforms only the latest workflow in the completed state',
    input: [
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.AwaitingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.AwaitingEnvironmentVariables,
        to_state: OneClickDeploymentState.Initialized,
      },
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.SufficientEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.SufficientEnvironmentVariables,
        to_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
        to_state: OneClickDeploymentState.Error,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.Error,
        to_state: OneClickDeploymentState.Initialized,
      },
      {
        id: 1,
        additional_info: {},
        from_state: OneClickDeploymentState.Initialized,
        to_state: OneClickDeploymentState.CloningGitRepository,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.CloningGitRepository,
        to_state: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ReadingEnvironmentVariables,
        to_state: OneClickDeploymentState.SufficientEnvironmentVariables,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.SufficientEnvironmentVariables,
        to_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
      },
      {
        id: 2,
        additional_info: {},
        from_state: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
        to_state: OneClickDeploymentState.Completed,
      },
    ],
    output: [
      {
        kind: 'success',
        step: OneClickDeploymentState.Initialized,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.CloningGitRepository,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.ReadingEnvironmentVariables,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.SufficientEnvironmentVariables,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.ApplyingMetadataMigrationsSeeds,
      },
      {
        kind: 'success',
        step: OneClickDeploymentState.Completed,
      },
    ],
  },
];

describe('transformStatelogToCLILog', () => {
  testCases.forEach(({ input, output, name, only = false }) => {
    const testFunc = only ? it.only : it;
    testFunc(name, () => {
      expect(transformStatelogToCLILog(input)).toEqual(output);
    });
  });
});

describe('getCliProgressState', () => {
  tc.forEach(({ input, output, name, only }) => {
    const testFunc = only ? it.only : it;
    testFunc(name, () => {
      expect(getCliProgressState(input)).toEqual(output);
    });
  });
});

describe('getSampleQueryUrl', () => {
  it('constructs query url correctly from git repo details', () => {
    expect(
      getSampleQueriesUrl({
        url: 'https://github.com/hasura/lux',
        branch: '2080-add-tests',
        hasuraDirectory: 'services',
      })
    ).toEqual(
      'https://raw.githubusercontent.com/hasura/lux/2080-add-tests/services/sample-requests.graphql'
    );
  });

  it('returns empty string for a non-url', () => {
    expect(
      getSampleQueriesUrl({
        url: 'non-url',
        branch: 'branch',
        hasuraDirectory: 'services',
      })
    ).toEqual('');
  });
});
