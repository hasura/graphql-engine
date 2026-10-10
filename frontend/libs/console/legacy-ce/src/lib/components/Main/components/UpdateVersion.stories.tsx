import type { Decorator, Meta, StoryObj } from '@storybook/react-webpack5';
import { expect, userEvent, waitFor, within } from 'storybook/test';
import { http, HttpResponse, delay } from 'msw';
import { AppContext, AppState } from '@hasura/shared/context';
import { EnvVars, LS_KEYS } from '@hasura/shared/types';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import {
  mockAppState,
  mockEnvVars,
} from '../../../../../.storybook/mockAppState';
import { UpdateVersion } from './UpdateVersion';

const baseUrl = 'http://localhost:8080';

const proEnvVars: EnvVars = {
  ...mockEnvVars,
  consoleType: 'pro',
  consoleMode: 'server',
  consoleId: 'console-id',
  isAdminSecretSet: true,
  isAdminSecretDisabled: false,
};

const withAppState = (overrides: Partial<AppState>): Decorator => {
  return (Story) => (
    <AppContext.Provider value={{ ...mockAppState, ...overrides }}>
      <Story />
    </AppContext.Provider>
  );
};

const catalogStateHandlers = [
  http.post(`${baseUrl}/v1/metadata`, async ({ request }) => {
    await delay(100);
    const body = (await request.json()) as { type: string };

    if (body.type === 'set_catalog_state') {
      return HttpResponse.json({ message: 'success' });
    }

    return HttpResponse.json({
      id: 'hasura-uuid',
      console_state: { disablePreReleaseUpdateNotifications: true },
    });
  }),
];

const meta: Meta<typeof UpdateVersion> = {
  component: UpdateVersion,
  title: 'components / Main / UpdateVersion',
  decorators: [ReactQueryDecorator()],
  args: {
    consoleState: {},
  },
  parameters: {
    msw: catalogStateHandlers,
  },
  // The banner stays hidden once dismissed for a version, so reset that
  // before every story.
  beforeEach: () => {
    window.localStorage.removeItem(LS_KEYS.versionUpdateCheckLastClosed);
  },
};

export default meta;

type Story = StoryObj<typeof UpdateVersion>;

export const StableUpdate: Story = {
  decorators: [
    withAppState({
      serverVersion: 'v2.44.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: '' },
    }),
  ],
};

export const PreReleaseUpdate: Story = {
  decorators: [
    withAppState({
      serverVersion: 'v2.44.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: 'v2.46.0-beta.1' },
    }),
  ],
};

export const PreReleaseOptedOut: Story = {
  args: {
    consoleState: { disablePreReleaseUpdateNotifications: true },
  },
  decorators: [
    withAppState({
      serverVersion: 'v2.44.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: 'v2.46.0-beta.1' },
    }),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(await canvas.findByText('v2.45.0')).toBeInTheDocument();
    await expect(canvas.queryByText('v2.46.0-beta.1')).not.toBeInTheDocument();
  },
};

export const UpToDate: Story = {
  decorators: [
    withAppState({
      serverVersion: 'v2.45.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: '' },
    }),
  ],
  play: async ({ canvasElement }) => {
    await expect(canvasElement).toBeEmptyDOMElement();
  },
};

export const NotOssConsole: Story = {
  decorators: [
    withAppState({
      envVars: proEnvVars,
      serverVersion: 'v2.44.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: '' },
    }),
  ],
  play: async ({ canvasElement }) => {
    await expect(canvasElement).toBeEmptyDOMElement();
  },
};

export const CloseBanner: Story = {
  decorators: [
    withAppState({
      serverVersion: 'v2.44.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: '' },
    }),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const version = await canvas.findByText('v2.45.0');

    // The close button is an icon-only span right after the banner content.
    const closeButton = canvasElement.querySelector('svg')?.parentElement;
    await expect(closeButton).toBeTruthy();
    await userEvent.click(closeButton as HTMLElement);

    await waitFor(() => expect(version).not.toBeInTheDocument());
    await expect(
      window.localStorage.getItem(LS_KEYS.versionUpdateCheckLastClosed),
    ).toContain('v2.45.0');
  },
};

export const OptOutOfPreRelease: Story = {
  decorators: [
    withAppState({
      serverVersion: 'v2.44.0',
      latestServerVersion: { latest: 'v2.45.0', prerelease: 'v2.46.0-beta.1' },
    }),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(
      await canvas.findByText('Opt out of pre-release notifications'),
    );

    await waitFor(() =>
      expect(canvas.queryByText('v2.46.0-beta.1')).not.toBeInTheDocument(),
    );
    await expect(
      await within(document.body).findByText(
        'Opted out of pre-release version release notifications',
      ),
    ).toBeInTheDocument();
  },
};
