import { expect } from '@storybook/jest';
import { Meta, StoryObj } from '@storybook/react';
import { screen, userEvent, within } from '@storybook/testing-library';
import isChromatic from 'chromatic/isChromatic';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { confirmAlert, dismissToast } from '../../../../utils/StoryUtils';
import { nativeQueryHandlers } from '../AddNativeQuery/mocks';
import { Routes } from '../constants';
import { LandingPage } from './LandingPage';
import { ConsoleTypeDecorator } from '../../../../storybook/decorators';

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
    return <LandingPage pathname={Routes.NativeQueries} />;
  },
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
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
    return <LandingPage pathname={Routes.NativeQueries} />;
  },
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: false } },
    }),
  },
};

export const NoModels: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: false, queries: true } },
    }),
  },
};

const testRemoveQueryAndModel = async ({
  canvasElement,
  removeResponse,
  removeButtonIndex = {
    logicalModels: 0,
    nativeQueries: 0,
  },
  skipConfirm = {
    logicalModels: false,
    nativeQueries: false,
  },
}: {
  canvasElement: HTMLElement;
  removeResponse?: {
    nativeQueries?: string;
    logicalModels?: string;
  };
  removeButtonIndex?: {
    logicalModels: number;
    nativeQueries: number;
  };
  skipConfirm?: {
    logicalModels: boolean;
    nativeQueries: boolean;
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
    )[removeButtonIndex.nativeQueries]
  );

  if (!skipConfirm.nativeQueries) {
    await confirmAlert({ confirmText: 'Remove', async: true });
  }

  if (removeResponse?.nativeQueries) {
    await expect(
      await screen.findByText(removeResponse.nativeQueries, { exact: false })
    ).toBeInTheDocument();

    await dismissToast();
  }

  await userEvent.click(await c.findByText('Logical Models', { exact: false }));

  await userEvent.click(
    (
      await c.findAllByText('Remove')
    )[removeButtonIndex.logicalModels]
  );

  if (!skipConfirm.logicalModels) {
    await confirmAlert({ confirmText: 'Remove', async: true });
  }

  if (removeResponse?.logicalModels) {
    await expect(
      await screen.findByText(removeResponse.logicalModels, { exact: false })
    ).toBeInTheDocument();

    await dismissToast();
  }
};

export const HappyPath: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '😊 Happy Path',
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
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

export const Referenced: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },
  name: '🚨 Cannot Remove Referenced Logical Model',
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
      removeButtonIndex: {
        nativeQueries: 0,
        logicalModels: 1, //select the 2nd one in the list
      },
      skipConfirm: {
        logicalModels: true,
        nativeQueries: false,
      },
      removeResponse: {
        logicalModels: `is referenced by 1 other entity(s).`,
      },
    });

    await confirmAlert({ confirmText: 'Ok', async: false });
  },
};

export const NotFound: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '🚨 Not found',
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
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
        logicalModels: `Logical model "hello_mssql_unused" not found in source "mssql".`,
      },
      removeButtonIndex: {
        nativeQueries: 0,
        logicalModels: 0,
      },
    });
  },
};

export const Disabled: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '🚨 Native Queries Disabled',
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
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
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '🚨 Native Queries Oss',
  decorators: [ConsoleTypeDecorator({ consoleType: 'oss' })],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
  },
};

export const Pro: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '🚨 Native Queries Pro',
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
  },
};

export const ProLite: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '🚨 Native Queries ProLite',
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro-lite' })],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: {
        postgres: { models: true, queries: true },
        mssql: { models: true, queries: true },
      },
      untrackNativeQueryResult: 'native_queries_disabled',
      untrackLogicalModelResult: 'native_queries_disabled',
    }),
  },
};

export const FeatureFlagDisabled: StoryObj<typeof LandingPage> = {
  render: args => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },

  name: '🚨 Native Queries FeatureFlagDisabled',
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
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
  },
};
