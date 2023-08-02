import { transformNeonIntegrationStatusToNeonBannerProps } from './utils';
import type { NeonIntegrationStatus } from './useNeonIntegration';
import type { Props as NeonBannerProps } from './components/Neon/NeonBanner';

const transformNeonIntegrationStatusTestCases: {
  name: string;
  input: NeonIntegrationStatus;
  output: NeonBannerProps;
}[] = [
  {
    name: 'transforms the idle state correctly',
    input: {
      status: 'idle',
      payload: {},
      action: jest.fn(),
    },
    output: {
      status: {
        status: 'default',
      },
      icon: undefined,
      buttonText: 'Connect Neon Database',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the authentication-loading state correctly',
    input: {
      status: 'authentication-loading',
      payload: {},
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Authenticating with Neon',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the authentication-success state correctly',
    input: {
      status: 'authentication-success',
      payload: {},
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Creating Database',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the neon-db-creation-loading state correctly',
    input: {
      status: 'neon-database-creation-loading',
      payload: {},
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Creating Database',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the neon-db-creation-success state correctly',
    input: {
      status: 'neon-database-creation-success',
      payload: {
        email: 'email@email.com',
        databaseUrl: 'dbUrl',
      },
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Connecting to Hasura',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the env-db-creation-loading state correctly',
    input: {
      status: 'env-var-creation-loading',
      payload: {
        databaseUrl: 'dbUrl',
        dataSourceName: 'dsName',
      },
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Connecting to Hasura',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the env-db-creation-success state correctly',
    input: {
      status: 'env-var-creation-success',
      payload: {
        databaseUrl: 'dbUrl',
        dataSourceName: 'dsName',
        envVar: 'envVarName',
      },
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Connecting to Hasura',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the hasura source creation loading state state correctly',
    input: {
      status: 'hasura-source-creation-loading',
      payload: {
        dataSourceName: 'dsName',
        envVar: 'envVarName',
        databaseUrl: 'dbUrl',
      },
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Connecting to Hasura',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms the hasura source creation success state state correctly',
    input: {
      status: 'hasura-source-creation-loading',
      payload: {
        dataSourceName: 'dsName',
        envVar: 'envVarName',
        databaseUrl: 'dbUrl',
      },
    },
    output: {
      status: {
        status: 'loading',
      },
      icon: undefined,
      buttonText: 'Connecting to Hasura',
      onClickConnect: jest.fn(),
    },
  },
];

const transformErrorStatesTestCases: {
  name: string;
  input: NeonIntegrationStatus;
  output: NeonBannerProps;
}[] = [
  {
    name: 'transforms authentication-error state correctly',
    input: {
      status: 'authentication-error',
      payload: {},
      action: jest.fn(),
      title: 'Error title',
      description: 'Error description',
    },
    output: {
      status: {
        status: 'error',
        errorTitle: 'Error title',
        errorDescription: 'Error description',
      },
      icon: 'refresh',
      buttonText: 'Try again',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms neon-db-creation-error state correctly',
    input: {
      status: 'neon-database-creation-error',
      payload: {},
      action: jest.fn(),
      title: 'Error title',
      description: 'Error description',
    },
    output: {
      status: {
        status: 'error',
        errorTitle: 'Error title',
        errorDescription: 'Error description',
      },
      icon: 'refresh',
      buttonText: 'Try again',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms env-var-creation-error state correctly',
    input: {
      status: 'env-var-creation-error',
      payload: {
        databaseUrl: 'dbUrl',
        dataSourceName: 'dsName',
      },
      action: jest.fn(),
      title: 'Error title',
      description: 'Error description',
    },
    output: {
      status: {
        status: 'loading',
      },
      buttonText: 'Connecting to Hasura',
      onClickConnect: jest.fn(),
    },
  },
  {
    name: 'transforms hasura-source-creation-error state correctly',
    input: {
      status: 'hasura-source-creation-error',
      payload: {
        databaseUrl: 'dbUrl',
        dataSourceName: 'dsName',
        envVar: 'envVar',
      },
      action: jest.fn(),
      title: 'Error title',
      description: 'Error description',
    },
    output: {
      status: {
        status: 'error',
        errorTitle: 'Error title',
        errorDescription: 'Error description',
      },
      icon: 'refresh',
      buttonText: 'Try again',
      onClickConnect: jest.fn(),
    },
  },
];

describe('transformNeonIntegrationStatusToNeonBannerProps', () => {
  transformNeonIntegrationStatusTestCases.forEach(t => {
    it(t.name, () => {
      const output = transformNeonIntegrationStatusToNeonBannerProps(t.input);

      // assert that the status of the output is as expected
      expect(output.status.status).toEqual(t.output.status.status);

      // assert that the icon of the output is as expected
      expect(output.icon).toEqual(t.output.icon);

      // assert that the button text is as expected
      expect(output.buttonText).toEqual(t.output.buttonText);

      // if `input` has a corresponding action, assert that the same action is presented to the user in banner props
      if ('action' in t.input) {
        output.onClickConnect();
        expect(t.input.action).toHaveBeenCalled();
      }
    });
  });

  transformErrorStatesTestCases.forEach(t => {
    it(t.name, () => {
      const output = transformNeonIntegrationStatusToNeonBannerProps(t.input);
      // assert that the status of the output is as expected
      expect(output.status).toEqual(t.output.status);

      // coerce the expected output and actual output to `any` type for
      // making assertions irrespective of the descriminant
      const actualOutput: any = output;
      const expectedOutput: any = t.output;

      // expect the button text of the output to be as expected
      expect(actualOutput.buttonText).toEqual(expectedOutput.buttonText);

      if ('action' in t.input) {
        // some errors are not presented to users as errors and they're masked with a loading status
        // loading status does not have an actionable, so we assert the right actionable only in
        // cases where we present the status as "error"
        if (expectedOutput.status.status === 'error') {
          actualOutput.onClickConnect();
          expect(t.input.action).toHaveBeenCalled();
        }
      }

      // assert the presented icon in the output is as expected
      expect(actualOutput.icon).toEqual(expectedOutput.icon);
    });
  });
});
