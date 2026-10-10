import type { Meta, StoryObj, Decorator } from '@storybook/react-webpack5';
import { userEvent, within } from 'storybook/test';
import {
  AppContext,
  AppState,
  AuthContext,
  AuthService,
} from '@hasura/shared/context';
import { EnvVars } from '@hasura/shared/types';
import { mockAppState, mockEnvVars } from '../../../../.storybook/mockAppState';
import Login from './Login';

const eeEnvVars: EnvVars = {
  ...mockEnvVars,
  consoleType: 'pro',
  consoleMode: 'server',
  consoleId: 'console-id',
  isAdminSecretSet: false,
};

const cliMissingAdminSecretEnvVars: EnvVars = {
  ...mockEnvVars,
  consoleMode: 'cli',
  consoleType: 'oss',
  adminSecret: '',
  apiHost: 'http://localhost',
  apiPort: '9693',
  cliUUID: 'cli-uuid',
  dataApiUrl: 'http://localhost:8080',
};

const cliInvalidAdminSecretEnvVars: EnvVars = {
  ...cliMissingAdminSecretEnvVars,
  adminSecret: 'wrong-secret',
};

const withAppState = (overrides: Partial<AppState>): Decorator => {
  return (Story) => (
    <AppContext.Provider value={{ ...mockAppState, ...overrides }}>
      <Story />
    </AppContext.Provider>
  );
};

const withAuthService = (overrides: Partial<AuthService<any>>): Decorator => {
  return (Story) => (
    <AuthContext.Provider
      value={{
        isAuthenticated: false,
        authType: 'admin-secret',
        getHeaders: async () => ({}),
        authenticate: () => Promise.resolve(true),
        logout: () => {},
        ...overrides,
      }}
    >
      <Story />
    </AuthContext.Provider>
  );
};

const meta: Meta<typeof Login> = {
  component: Login,
  title: 'components / Login',
};

export default meta;
type Story = StoryObj<typeof Login>;

export const Default: Story = {
  decorators: [
    withAuthService({
      authenticate: () => new Promise(() => {}),
    }),
  ],
};

export const Loading: Story = {
  decorators: [
    withAuthService({
      authenticate: () => new Promise(() => {}),
    }),
  ],
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement);

    await step('Enter password and submit', async () => {
      await userEvent.type(
        canvas.getByPlaceholderText('Enter admin-secret'),
        'my-secret',
      );
      await userEvent.click(canvas.getByText('Enter'));
    });
  },
};

export const AuthenticationError: Story = {
  decorators: [
    withAuthService({
      authenticate: () => Promise.reject(new Error('invalid admin-secret')),
    }),
  ],
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement);

    await step('Enter password and submit', async () => {
      await userEvent.type(
        canvas.getByPlaceholderText('Enter admin-secret'),
        'wrong-secret',
      );
      await userEvent.click(canvas.getByText('Enter'));
    });
  },
};

export const EEConsole: Story = {
  decorators: [
    withAppState({ envVars: eeEnvVars }),
    withAuthService({
      authenticate: () => new Promise(() => {}),
    }),
  ],
};

export const CliModeMissingAdminSecret: Story = {
  decorators: [withAppState({ envVars: cliMissingAdminSecretEnvVars })],
};

export const CliModeInvalidAdminSecret: Story = {
  decorators: [withAppState({ envVars: cliInvalidAdminSecretEnvVars })],
};
