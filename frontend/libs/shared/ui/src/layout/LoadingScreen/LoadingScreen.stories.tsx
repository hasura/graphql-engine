import { StoryObj, Meta } from '@storybook/react-webpack5';
import { expect, screen } from 'storybook/test';

import { LoadingScreen } from './LoadingScreen';
import { LoadingScreenError, LoadingScreenTitle } from './LoadingScreenStatus';

export default {
  title: 'layout/LoadingScreen',
  component: LoadingScreen,
  parameters: {
    docs: {
      description: {
        component: `Full-page status screen with the animated Hasura logo (or the static logo on error).
Compose it with \`LoadingScreenTitle\` while work is in progress and \`LoadingScreenError\` when it fails.`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    // LoadingScreen is `position: fixed`; the transform makes this box its
    // containing block, so each story stays inside its own frame in Docs.
    (Story) => (
      <div className="relative h-[420px] transform-gpu">{Story()}</div>
    ),
  ],
} as Meta<typeof LoadingScreen>;

export const Loading: StoryObj<typeof LoadingScreen> = {
  name: '🧰 Loading',
  render: () => (
    <LoadingScreen>
      <LoadingScreenTitle title="Validating your credentials..." />
    </LoadingScreen>
  ),
  play: async () => {
    await expect(await screen.findByLabelText('Loading')).toBeVisible();
    await expect(
      screen.getByText('Validating your credentials...'),
    ).toBeVisible();
  },
};

export const ErrorWithMessage: StoryObj<typeof LoadingScreen> = {
  name: '🎭 Error - with message and link',
  render: () => (
    <LoadingScreen isError>
      <LoadingScreenError
        title="Authentication failed"
        message="The authorization code has expired. Please log in again."
        link="/login"
        linkText="Back to login"
      />
    </LoadingScreen>
  ),
  play: async () => {
    await expect(screen.getByAltText('Hasura Logo')).toBeVisible();
    await expect(screen.getByText('Authentication failed')).toBeVisible();
    await expect(
      screen.getByRole('button', { name: 'Back to login' }),
    ).toBeVisible();
  },
};

export const ErrorTitleOnly: StoryObj<typeof LoadingScreen> = {
  name: '🎭 Error - title only',
  render: () => (
    <LoadingScreen isError>
      <LoadingScreenError title="Something went wrong" />
    </LoadingScreen>
  ),
  play: async () => {
    await expect(screen.getByText('Something went wrong')).toBeVisible();
    await expect(screen.queryByRole('button')).not.toBeInTheDocument();
  },
};
