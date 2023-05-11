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
  return <LandingPage />;
};

Basic.parameters = {
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
  return <LandingPage />;
};

NoQueries.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: true, queries: false } },
  }),
};

export const NoModels: ComponentStory<typeof LandingPage> = args => {
  return <LandingPage />;
};

NoModels.parameters = {
  msw: nativeQueryHandlers({
    metadataOptions: { postgres: { models: false, queries: true } },
  }),
};

const remove = async ({
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

  await confirmAlert();

  if (removeResponse?.nativeQueries) {
    await expect(
      await screen.findByText(removeResponse.nativeQueries)
    ).toBeInTheDocument();

    await dismissToast();
  }

  await userEvent.click(await c.findByText('Logical Models (4)'));

  await userEvent.click((await c.findAllByText('Remove'))[0]);

  await confirmAlert();

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
  return <LandingPage />;
};

HappyPath.storyName = '😊 Happy Path';

HappyPath.parameters = {
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
  remove({
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
  return <LandingPage />;
};

NotFound.storyName = '🚨 Not found';

NotFound.parameters = {
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
  remove({
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
  return <LandingPage />;
};

Disabled.storyName = '🚨 Native Queries Disabled';

Disabled.parameters = {
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
  remove({
    canvasElement,
    removeResponse: {
      nativeQueries: 'NativeQueries is disabled!',
      logicalModels: 'NativeQueries is disabled!',
    },
  });
};
