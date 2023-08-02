import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import isChromatic from 'chromatic/isChromatic';
import { expect } from '@storybook/jest';
import { screen, userEvent, within } from '@storybook/testing-library';
import { confirmAlert, dismissToast } from '../../../../utils/StoryUtils';
import { nativeQueryHandlers } from '../AddNativeQuery/mocks';
import { LandingPage } from './LandingPage';

export default {
  component: LandingPage,
  decorators: [ReactQueryDecorator()],
  parameters: {
    layout: 'fullscreen',
    chromatic: { disableSnapshot: true },
  },
} as Meta<typeof LandingPage>;

export const Basic: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  parameters: {
    consoleType: 'pro',
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'success',
      untrackLogicalModelResult: 'success',
    }),
  },
};

export const NoQueries: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  parameters: {
    consoleType: 'pro',
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: false } },
    }),
  },
};

export const NoModels: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  parameters: {
    consoleType: 'pro',
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: false, queries: true } },
    }),
  },
};

const testRemoveQueryAndModel = async ({
  canvasElement,
  removeResponse,
}: {
  canvasElement: HTMLElement;
  removeResponse?: {
    nativeQueries?: string;
    logicalModels?: string;
  };
}) => {
  if (isChromatic()) {
    return;
  }

  const c = within(canvasElement);

  await c.findAllByText('Native Queries', { exact: false }, { timeout: 3000 });

  await userEvent.click(
    (
      await c.findAllByText('Remove', undefined, { timeout: 3000 })
    )[0]
  );

  await confirmAlert('Remove');

  if (removeResponse?.nativeQueries) {
    await expect(
      await screen.findByText(removeResponse.nativeQueries)
    ).toBeInTheDocument();

    await dismissToast();
  }

  await userEvent.click(await c.findByText('Logical Models', { exact: false }));

  await userEvent.click((await c.findAllByText('Remove'))[0]);

  await confirmAlert('Remove');

  if (removeResponse?.logicalModels) {
    await expect(
      await screen.findByText(removeResponse.logicalModels)
    ).toBeInTheDocument();

    await dismissToast();
  }
};

export const HappyPath: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '😊 Happy Path',

  parameters: {
    consoleType: 'pro',
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'success',
      untrackLogicalModelResult: 'success',
    }),
  },

  play: async ({ canvasElement }) => {
    await testRemoveQueryAndModel({
      canvasElement,
    });
  },
};

export const NotFound: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Not found',

  parameters: {
    consoleType: 'pro',
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'not_found',
      untrackLogicalModelResult: 'not_found',
    }),
  },

  play: async ({ canvasElement }) => {
    await testRemoveQueryAndModel({
      canvasElement,
      removeResponse: {
        nativeQueries: `Native query "hello_mssql_function" not found in source "mssql".`,
        logicalModels: `Logical model "hello_mssql" not found in source "mssql".`,
      },
    });
  },
};

export const Disabled: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Native Queries Disabled',

  parameters: {
    consoleType: 'pro',
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
  },

  play: async ({ canvasElement }) => {
    await testRemoveQueryAndModel({
      canvasElement,
      removeResponse: {
        nativeQueries: 'NativeQueries is disabled!',
        logicalModels: 'NativeQueries is disabled!',
      },
    });
  },
};

export const Oss: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Native Queries Oss',

  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
    consoleType: 'oss',
  },
};

export const Pro: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Native Queries Pro',

  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
    consoleType: 'pro',
  },
};

export const ProLite: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Native Queries ProLite',

  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
    consoleType: 'pro-lite',
  },
};

export const FeatureFlagDisabled: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Native Queries FeatureFlagDisabled',

  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
      enabledFeatureFlag: false,
    }),
    consoleType: 'pro',
  },
};

export const StillBeingUsed: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname="/data/native-queries" />;
  },

  name: '🚨 Logical Model Remove Conflict',

  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackLogicalModelResult: 'still_being_used',
    }),
    consoleType: 'pro',
  },

  play: async ({ canvasElement }) => {
    await testRemoveQueryAndModel({
      canvasElement,
      removeResponse: {
        logicalModels:
          'Custom type "hello_mssql" still being used by native query "hello_mssql_function".',
      },
    });
  },
};
