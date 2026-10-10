import { Meta, StoryObj } from '@storybook/react-webpack5';
import {
  ConsoleTypeDecorator,
  ReactQueryDecorator,
} from '@hasura/shared/testing';
import { nativeQueryHandlers } from '../AddNativeQuery/mocks';
import { Routes } from '../constants';
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
    return <LandingPage pathname={Routes.NativeQueries} />;
  },
  decorators: [ConsoleTypeDecorator({ consoleType: 'pro' })],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: false, queries: true } },
    }),
  },
};

export const Oss: StoryObj<typeof LandingPage> = {
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
  render: (args) => {
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
