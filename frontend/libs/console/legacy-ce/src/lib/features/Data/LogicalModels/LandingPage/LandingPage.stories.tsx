import { ComponentMeta, ComponentStory } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';

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
    // Until we find out why the delete confirm modal is sometimes present in the screnshots
    chromatic: { disableSnapshot: true },
  },
} as ComponentMeta<typeof LandingPage>;

export const Basic: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

Basic.parameters = {
  consoleType: 'pro',
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'success',
    untrackLogicalModelResult: 'success',
  }),
};

export const NoQueries: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

NoQueries.parameters = {
  consoleType: 'pro',
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: true, queries: false } },
  }),
};

export const NoModels: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

NoModels.parameters = {
  consoleType: 'pro',
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: false, queries: true } },
  }),
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
  const c = within(canvasElement);
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

  await userEvent.click(await c.findByText('Logical Models (4)'));

  await userEvent.click((await c.findAllByText('Remove'))[0]);

  await confirmAlert('Remove');

  if (removeResponse?.logicalModels) {
    await expect(
      await screen.findByText(removeResponse.logicalModels)
    ).toBeInTheDocument();

    await dismissToast();
  }
};

/**
 *
 *
 * When both requests to remove return success
 *
 *
 */

export const HappyPath: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

HappyPath.storyName = '😊 Happy Path';

HappyPath.parameters = {
  consoleType: 'pro',
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'success',
    untrackLogicalModelResult: 'success',
  }),
};

HappyPath.play = async ({ canvasElement }) => {
  testRemoveQueryAndModel({
    canvasElement,
  });
};

/**
 *
 *
 * When both requests to remove return a not found respond
 *
 *
 */

export const NotFound: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

NotFound.storyName = '🚨 Not found';

NotFound.parameters = {
  consoleType: 'pro',
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'not_found',
    untrackLogicalModelResult: 'not_found',
  }),
};

NotFound.play = async ({ canvasElement }) => {
  testRemoveQueryAndModel({
    canvasElement,
    removeResponse: {
      nativeQueries: `Native query "hello_mssql_function" not found in source "mssql".`,
      logicalModels: `Logical model "hello_mssql" not found in source "mssql".`,
    },
  });
};

/**
 *
 *
 * Disabled
 *
 */
export const Disabled: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

Disabled.storyName = '🚨 Native Queries Disabled';

Disabled.parameters = {
  consoleType: 'pro',
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'native_queries_disabled',
    untrackLogicalModelResult: 'native_queries_disabled',
  }),
};

Disabled.play = async ({ canvasElement }) => {
  testRemoveQueryAndModel({
    canvasElement,
    removeResponse: {
      nativeQueries: 'NativeQueries is disabled!',
      logicalModels: 'NativeQueries is disabled!',
    },
  });
};

/**
 *
 *
 * Oss
 *
 */
export const Oss: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

Oss.storyName = '🚨 Native Queries Oss';

Oss.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'native_queries_disabled',
    untrackLogicalModelResult: 'native_queries_disabled',
  }),
  consoleType: 'oss',
};

/**
 *
 *
 * Pro
 *
 */
export const Pro: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

Pro.storyName = '🚨 Native Queries Pro';

Pro.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'native_queries_disabled',
    untrackLogicalModelResult: 'native_queries_disabled',
  }),
  consoleType: 'pro',
};

/**
 *
 *
 * ProLite
 *
 */
export const ProLite: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

ProLite.storyName = '🚨 Native Queries ProLite';

ProLite.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackNativeQueryResult: 'native_queries_disabled',
    untrackLogicalModelResult: 'native_queries_disabled',
  }),
  consoleType: 'pro-lite',
};

/**
 *
 *
 * FeatureFlagDisabled
 *
 */
export const FeatureFlagDisabled: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

FeatureFlagDisabled.storyName = '🚨 Native Queries FeatureFlagDisabled';

FeatureFlagDisabled.parameters = {
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
};

/**
 *
 *
 * Logical Model still being used by some Native Query
 *
 */
export const StillBeingUsed: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage pathname="/data/native-queries" />;
};

StillBeingUsed.storyName = '🚨 Logical Model Remove Conflict';

StillBeingUsed.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: {
      postgres: { models: true, queries: true },
      mssql: { models: true, queries: true },
    },
    untrackLogicalModelResult: 'still_being_used',
  }),
  consoleType: 'pro',
};

StillBeingUsed.play = async ({ canvasElement }) => {
  testRemoveQueryAndModel({
    canvasElement,
    removeResponse: {
      logicalModels:
        'Custom type "hello_mssql" still being used by native query "hello_mssql_function".',
    },
  });
};
