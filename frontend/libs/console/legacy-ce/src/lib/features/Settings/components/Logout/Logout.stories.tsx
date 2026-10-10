import type { Meta, StoryObj, Decorator } from '@storybook/react-webpack5';
import { AuthContext, AuthService } from '@hasura/shared/context';

import Logout from './Logout';

const withAuthService = (overrides: Partial<AuthService<any>>): Decorator => {
  return (Story) => (
    <AuthContext.Provider
      value={{
        isAuthenticated: true,
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

export default {
  title: 'Features/Settings/Logout',
  component: Logout,
  parameters: {
    docs: { disable: true },
  },
  decorators: [withAuthService({})],
} as Meta<typeof Logout>;

export const Default: StoryObj<typeof Logout> = {
  name: '💠 Demo Logout',
};
