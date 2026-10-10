import { isCommonAssetRequest } from 'msw';
import type { SetupWorker } from 'msw/browser';
import { mswLoader } from 'msw-storybook-addon/csf3';
import { MemoryRouter } from 'react-router';
import theme from './theme';
import { useLayoutEffect } from 'react';
import { AlertProvider, AppTheme, useAppearance } from '@hasura/shared/ui';
import { Decorator } from '@storybook/react';
import { AppContext } from '@hasura/shared/context';
import { mockAppState } from './mockAppState';

import '../src/lib/theme/tailwind.css';

// msw-storybook-addon's default setup still passes MSW 2's `onUnhandledRequest`,
// which MSW 3 ignores (renamed to `onUnhandledFrame`). Same setup, MSW 3 option.
const setupMsw = async (): Promise<SetupWorker> => {
  const { setupWorker } = await import('msw/browser');
  const worker = setupWorker();
  await worker.start({
    quiet: true,
    onUnhandledFrame({ frame, defaults }) {
      if (frame.protocol === 'http') {
        const { request } = frame.data as { request: Request };
        if (
          isCommonAssetRequest(request) ||
          /\.eot$|\.mdx$|sb-common-assets|__webpack_hmr|iframe.html|sb-vite|@vite|@react-refresh|\/virtual:|\.stories\./.test(
            request.url,
          )
        ) {
          return;
        }
      }
      defaults.warn();
    },
  });
  return worker;
};

export const loaders = [mswLoader(setupMsw)];

export const parameters = {
  // Add a minimum of 300 ms of delay for ui testing
  actions: { argTypesRegex: '^on.*' },
  options: {
    storySort: {
      order: ['Design system', 'Dev', 'Components', 'Hooks'],
    },
  },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
  darkMode: {
    dark: { ...theme.dark },
    light: { ...theme.light },
  },
};

// Toolbar switch for the console appearance (light/dark).
export const globalTypes = {
  appearance: {
    description: 'Console appearance (light/dark mode)',
    toolbar: {
      title: 'Appearance',
      icon: 'mirror',
      items: [
        { value: 'light', title: 'Light', icon: 'sun' },
        { value: 'dark', title: 'Dark', icon: 'moon' },
      ],
      dynamicTitle: true,
    },
  },
};

export const initialGlobals = {
  appearance: 'light',
};

// Pushes the toolbar's appearance into the app's ThemeProvider (from AppTheme),
// so stories go through the same code path as the real console.
const AppearanceSync = ({ appearance }: { appearance: 'light' | 'dark' }) => {
  const { setAppearance } = useAppearance();
  useLayoutEffect(() => {
    setAppearance(appearance);
  }, [appearance, setAppearance]);
  return null;
};

export const decorators: Decorator[] = [
  (story, { parameters }) => {
    if (!parameters.mockdate) {
      return story();
    }

    const mockedDate = new Date(parameters.mockdate).toISOString();

    return (
      <div>
        {story()}
        <div
          style={{
            position: 'fixed',
            bottom: 0,
            right: 0,
            background: 'rgba(0, 0, 0, 0.15)',
            padding: '5px',
            lineHeight: 1,
          }}
        >
          <span className={'font-bold'}>Mocked date:</span> {mockedDate}
        </div>
      </div>
    );
  },
  (Story, { globals }) => {
    document.body.classList.add('hasura-tailwind-on');
    return (
      <AlertProvider>
        <AppearanceSync
          appearance={globals['appearance'] === 'dark' ? 'dark' : 'light'}
        />
        <div>
          <div className={'bg-legacybg'}>
            <Story />
          </div>
        </div>
      </AlertProvider>
    );
  },
  (Story) => (
    <AppTheme>
      <AppContext.Provider value={mockAppState}>
        <MemoryRouter>
          <Story />
        </MemoryRouter>
      </AppContext.Provider>
    </AppTheme>
  ),
];

export const argTypes = {
  disableSnapshotTesting: {
    table: {
      disable: true,
    },
  },
};
